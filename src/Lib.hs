{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Network.Socket as NS
import Network.Socket.ByteString (recv,sendAll)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C

--import qualified Network.Simple.TCP as TCP

runClient :: IO ()
runClient = NS.withSocketsDo $ do
        addr <- resolve "127.0.0.1" "12482"
        E.bracket (open addr) NS.close talk
    where
        resolve host port = do
            let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
            addr:_ <- NS.getAddrInfo (Just hints) (Just host) (Just port)
            return addr
        open addr = do
            sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
            NS.connect sock $ NS.addrAddress addr
            return sock
        talk sock = do
            sendAll sock "Hello, world!"
            msg <- recv sock 1024
            putStr "Received: "
            C.putStrLn msg

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
    NS.close conn
