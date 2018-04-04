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

data CharacterData = CharacterData deriving Show

evalServerInput :: String -> Ooey.OoeyIO -> STM.TVar (Map.Map String ConnectionData) -> IO ()
evalServerInput input_ userIO characterMap = do
    currentCMap <- STM.atomically $ STM.readTVar characterMap
    sequence $ map (flip sendAll (C.pack input_) . connSocket) (Map.elems currentCMap)
    Ooey.ooeyPutStr userIO $ Right input_


evalClientInput :: ConnectionData -> String -> Ooey.OoeyIO -> STM.TVar (Map.Map String ConnectionData) -> IO ()
evalClientInput conn input_ userIO characterMap = do
    currentCMap <- STM.atomically $ STM.readTVar characterMap
    --sequence $ map (STM.atomically . flip STM.writeTChan input_) (Map.elems currentCMap)
    sequence $ map (flip sendAll (C.pack input_) . connSocket) (Map.elems currentCMap)
    Ooey.ooeyPutStr userIO $ Right input_

serverLoop :: Ooey.OoeyIO -> STM.TChan (Maybe ConnectionData, String) -> STM.TVar (Map.Map String ConnectionData) -> IO ()
serverLoop userIO inputChannel characterMap = do
    (connData, input_) <- STM.atomically $ STM.readTChan inputChannel
    (maybe evalServerInput evalClientInput connData) input_ userIO characterMap
    serverLoop userIO inputChannel characterMap
    
broadcast :: STM.TChan (Maybe ConnectionData, String) -> Maybe ConnectionData -> String -> IO ()
broadcast inputChannel connData msg = STM.atomically $ STM.writeTChan inputChannel (connData, msg)

runServerLoop :: Ooey.OoeyIO -> NS.Socket -> IO ()
runServerLoop userIO sock = do
    characterMap <- STM.atomically $STM.newTVar Map.empty
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
                    broadcast inputChannel Nothing ("DM:" ++ serverUserInput)
                    inputLoop characterMap inputChannel
          acceptLoop characterMap inputChannel = do
            (conn, peer) <- NS.accept sock
            Ooey.ooeyPutStr userIO $ Right $ "We got a connection! From: " ++ (show peer)
            let connData = ConnectionData (show peer) conn Nothing
            STM.atomically $ STM.modifyTVar characterMap (Map.insert (show peer) connData)
            forkFinally (connectionLoop connData (conn, peer) inputChannel) (\_ -> STM.atomically $ STM.modifyTVar characterMap (Map.delete (show peer)))
            acceptLoop characterMap inputChannel
          connectionLoop connData (conn, peer) inputChannel = do 
            msg <- recv conn 1024
            if msg == "" 
                then do
                    Ooey.ooeyPutStr userIO $ Right $ "Connection with " ++ (show peer) ++ " has ended"
                    NS.close conn
                else do
                    --Ooey.ooeyPutStr userIO $ Right $ "Received from(" ++ (show peer) ++ "): " ++ (C.unpack msg)
                    STM.atomically $ STM.writeTChan inputChannel (Just connData, (C.unpack msg))
                    --sendAll conn msg
                    connectionLoop connData (conn, peer) inputChannel