{-# LANGUAGE OverloadedStrings #-}
module Lib where

import qualified Lib.UI as Ooey 
import qualified Lib.Server as Server

import qualified Network.Socket as NS
import Network.Socket.ByteString (recv,sendAll)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Control.Concurrent.STM as STM
import System.IO
import Control.Concurrent(forkIO, forkFinally)

import qualified Data.Map as Map
import Data.Char(toUpper)

--import qualified Network.Simple.TCP as TCP

type ErrorMsg = String

ooeyTerminalWithSocket :: (String -> IO (Either Ooey.ExitMessage String)) -> (String -> IO String) -> ErrorMsg -> Ooey.OoeyIO -> NS.Socket -> IO ()
ooeyTerminalWithSocket recvFunc sendFunc errMsg userIO socket = do
        --E.catch ((forkIO $ sendInput userIO) >> return ()) 
            --(\x -> return ())
        forkIO $ sendInput userIO
        forkIO $ recvOutput userIO
        return ()
    where sendInput :: Ooey.OoeyIO ->  IO ()
          sendInput userIO = do 
            userInput <- E.try (Ooey.ooeyGetLine userIO) :: IO (Either E.SomeException String)
            funcyUserInput <- either (const $ return errMsg) sendFunc userInput
            sendAll socket $ C.pack $ funcyUserInput
            either (const $ return ()) (const $ sendInput userIO) userInput

          recvOutput :: Ooey.OoeyIO -> IO ()
          recvOutput userIO = do
            connectionInput <- recvFunc =<< C.unpack <$> recv socket 2048
            Ooey.ooeyPutStr userIO $ connectionInput --recvFunc $ C.unpack $ connectionInput
            recvOutput userIO

clientRecvFunc :: String -> IO (Either Ooey.ExitMessage String)
clientRecvFunc "" = return $ Left "Connection closed"
clientRecvFunc "QUIT" = return $ Left "quitting..."
clientRecvFunc str = return $ Right str

clientSendFunc :: String -> IO String 
clientSendFunc = return . map toUpper

runClient :: IO ()
runClient = NS.withSocketsDo $ do
        addr <- resolve "127.0.0.1" "12482"
        E.bracket (open addr) NS.close $ \socket -> do 
            Ooey.withOoeyTerminal Ooey.defaultOoeyConfig $ \userIO -> do 
                ooeyTerminalWithSocket clientRecvFunc clientSendFunc "quit" userIO socket 
    where
        resolve host port = do
            let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
            addr:_ <- NS.getAddrInfo (Just hints) (Just host) (Just port)
            return addr
        open addr = do
            sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
            NS.connect sock $ NS.addrAddress addr
            return sock

runServer :: IO ()
runServer = Server.runServer