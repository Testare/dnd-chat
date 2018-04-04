{-# LANGUAGE OverloadedStrings #-}
module Lib.Server where

import Lib.UI as Ooey
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv,sendAll)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C

import Control.Concurrent.STM as STM
import System.IO
import Control.Concurrent(forkIO, forkFinally)

import qualified Data.Map as Map

runServer :: IO ()
runServer = NS.withSocketsDo $ Ooey.withOoeyTerminal Ooey.defaultOoeyConfig $ \userIO -> do
    address:_ <- NS.getAddrInfo (
        Just (NS.defaultHints 
                { NS.addrFlags = [NS.AI_PASSIVE]
                , NS.addrSocketType = NS.Stream 
                }))
        Nothing 
        (Just "12482")
    Ooey.ooeyPutStr userIO $ Right $ "Starting server at: [" ++ (show address) ++ "]"
    E.bracket (openServerSocket address) closeServerSocket (runServerLoop userIO) 
    Ooey.ooeyGetLine userIO
    return ()

openServerSocket :: NS.AddrInfo -> IO NS.Socket
openServerSocket address = do
    sock <- NS.socket (NS.addrFamily address) (NS.addrSocketType address) (NS.addrProtocol address)
    NS.setSocketOption sock NS.ReuseAddr 1
    NS.bind sock (NS.addrAddress address)
    NS.listen sock 5
    return sock

closeServerSocket :: NS.Socket -> IO ()
closeServerSocket = NS.close

data ConnectionData = ConnectionData 
                        { address :: String
                        , connSocket :: NS.Socket
                        , character :: Maybe CharacterData
                        } deriving Show

data CharacterData = CharacterData 
                        { characterName :: String 
                        } deriving (Show, Read)

evalServerInput :: String -> Ooey.OoeyIO -> STM.TVar (Map.Map String ConnectionData) -> IO ()
evalServerInput input_ userIO characterMap = do
    let msg = case input_ of
                'm':x -> "DM: " ++ x
                'w':x -> x
                'q':x -> x
    currentCMap <- STM.atomically $ STM.readTVar characterMap
    sequence $ map (flip sendAll (C.pack msg) . connSocket) (Map.elems currentCMap)
    Ooey.ooeyPutStr userIO $ Right msg

evalClientInput :: ConnectionData -> String -> Ooey.OoeyIO -> STM.TVar (Map.Map String ConnectionData) -> IO ()
evalClientInput conn input_ userIO characterMap = do
    case input_ of
        'm':sg ->  do
            currentCMap <- STM.atomically $ STM.readTVar characterMap
            let maybeName = Map.lookup (address conn) currentCMap >>= character
            --sequence $ map (STM.atomically . flip STM.writeTChan input_) (Map.elems currentCMap)
            let msg = (maybe "ANON" characterName maybeName) ++ (':':' ':sg)
            sequence $ map (flip sendAll (C.pack msg) . connSocket) (Map.elems currentCMap)
            Ooey.ooeyPutStr userIO $ Right msg
        'q':_ -> do 
            sendAll (connSocket conn) (C.pack input_)

serverLoop :: Ooey.OoeyIO -> STM.TChan (Maybe ConnectionData, String) -> STM.TVar (Map.Map String ConnectionData) -> IO ()
serverLoop userIO inputChannel characterMap = do
    (connData, input_) <- STM.atomically $ STM.readTChan inputChannel
    (maybe evalServerInput evalClientInput connData) input_ userIO characterMap
    serverLoop userIO inputChannel characterMap
    
broadcast :: STM.TChan (Maybe ConnectionData, String) -> Maybe ConnectionData -> String -> IO ()
broadcast inputChannel connData msg = STM.atomically $ STM.writeTChan inputChannel (connData, msg)

runServerLoop :: Ooey.OoeyIO -> NS.Socket -> IO ()
runServerLoop userIO sock = do
    characterMap <- STM.atomically $ STM.newTVar Map.empty
    inputChannel <- STM.atomically $ STM.newTChan 
    forkIO $ acceptLoop characterMap inputChannel
    forkIO $ serverLoop userIO inputChannel characterMap
    inputLoop characterMap inputChannel
    where inputLoop characterMap inputChannel = do
            serverUserInput <- Ooey.ooeyGetLine userIO
            case serverUserInput of
                "quit" -> Ooey.ooeyPutStr userIO (Left "Goodbye!")
                ":q" -> Ooey.ooeyPutStr userIO (Left "Goodbye!")
                ":c" -> do
                    currentConnectionData <- STM.atomically $ STM.readTVar characterMap
                    Ooey.ooeyPutStr userIO (Right $ show currentConnectionData)
                    inputLoop characterMap inputChannel
                _ -> do
                    broadcast inputChannel Nothing ('m':serverUserInput)
                    inputLoop characterMap inputChannel
          acceptLoop characterMap inputChannel = do
            (conn, peer) <- NS.accept sock
            Ooey.ooeyPutStr userIO $ Right $ "We got a connection! From: " ++ (show peer)
            let connData = ConnectionData (show peer) conn Nothing
            STM.atomically $ STM.modifyTVar characterMap (Map.insert (show peer) connData)
            forkFinally (connectionLoop connData (conn, (show peer)) characterMap inputChannel) (\_ -> STM.atomically $ STM.modifyTVar characterMap (Map.delete (show peer)))
            acceptLoop characterMap inputChannel
          connectionLoop :: ConnectionData -> (NS.Socket, String) -> STM.TVar (Map.Map String ConnectionData) -> STM.TChan (Maybe ConnectionData, String) -> IO ()
          connectionLoop connData (conn, peer) characterMap inputChannel = do 
            msg <- C.unpack <$> recv conn 1024
            case msg of
                "" -> do
                    Ooey.ooeyPutStr userIO $ Right $ "Connection with " ++ peer ++ " has ended"
                    NS.close conn
                "q" -> do 
                    currentCMap <- STM.atomically $ STM.readTVar characterMap
                    let maybeName = characterName <$> (Map.lookup peer currentCMap >>= character)
                    broadcast inputChannel Nothing $ maybe "q[--- has left]" (\nm -> "q[" ++ nm ++ " has left]") maybeName
                    broadcast inputChannel (Just connData) $ "qGoodbye!"
                'm':sg -> do
                    --Ooey.ooeyPutStr userIO $ Right $ "Received from(" ++ (show peer) ++ "): " ++ (C.unpack msg)
                    broadcast inputChannel (Just connData) msg
                    connectionLoop connData (conn, peer) characterMap inputChannel
                'c':cdat -> do
                    let charData = read cdat
                    let cData = connData {character = Just charData}
                    STM.atomically $ STM.modifyTVar characterMap $ Map.insert peer cData
                    broadcast inputChannel Nothing $ "w[" ++ (characterName $ charData) ++ " has joined]"
                    connectionLoop cData (conn, peer) characterMap inputChannel