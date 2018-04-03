{-# LANGUAGE OverloadedStrings #-}
module Lib where

import qualified Lib.UI as Ooey 

import qualified Network.Socket as NS
import Network.Socket.ByteString (recv,sendAll)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Control.Concurrent.STM as STM

--import qualified Network.Simple.TCP as TCP

runClient :: IO ()
runClient = NS.withSocketsDo $ do
        addr <- resolve "127.0.0.1" "12482"
        Ooey.withOoeyTerminal Ooey.defaultOoeyConfig $ \userIO -> do 
            E.bracket (open addr) NS.close (talk userIO)
            userSays2 <- Ooey.ooeyGetLine userIO 
            Ooey.ooeyPutStr userIO $ Right $ "U: " ++ userSays2
            Ooey.ooeyPutStr userIO $ Left "Time to go, bye!"
    where
        resolve host port = do
            let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
            addr:_ <- NS.getAddrInfo (Just hints) (Just host) (Just port)
            return addr
        open addr = do
            sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
            NS.connect sock $ NS.addrAddress addr
            return sock
        talk userIO sock = do
            Ooey.ooeyPutStr userIO $ Right "What should you say?"
            userSays <- Ooey.ooeyGetLine userIO
            sendAll sock $ C.pack userSays --"Hello, world!"
            msg <- recv sock 1024
            Ooey.ooeyPutStr userIO $ Right (C.unpack msg)
            return ()
            --STM.atomically $ STM.writeTChan userOut $ Left (C.unpack msg)
            --putStr "Received: "
            --C.putStrLn msg

runServer :: IO ()
runServer = NS.withSocketsDo $ do
    address:_ <- NS.getAddrInfo (
        Just (NS.defaultHints 
                { NS.addrFlags = [NS.AI_PASSIVE]
                , NS.addrSocketType = NS.Stream 
                }))
        Nothing 
        (Just "12482")
    putStrLn $ show address
    E.bracket (openServerSocket address) closeServerSocket runServerLoop


openServerSocket :: NS.AddrInfo -> IO NS.Socket
openServerSocket address = do
    sock <- NS.socket (NS.addrFamily address) (NS.addrSocketType address) (NS.addrProtocol address)
    NS.setSocketOption sock NS.ReuseAddr 1
    NS.bind sock (NS.addrAddress address)
    NS.listen sock 5
    return sock

closeServerSocket :: NS.Socket -> IO ()
closeServerSocket = NS.close

runServerLoop :: NS.Socket -> IO ()
runServerLoop sock = do
    (conn, peer) <- NS.accept sock
    putStrLn $ "We got a connection! From: " ++ (show peer)
    msg <- recv conn 1024
    putStr "Received: "
    C.putStrLn msg
    sendAll conn "Goodbye scrub!"
