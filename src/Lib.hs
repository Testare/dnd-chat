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
import System.Environment(getArgs)

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
clientRecvFunc ('q':quitMessage) = return $ Left quitMessage
clientRecvFunc str = return $ Right str

clientSendFunc :: String -> IO String 
clientSendFunc ":q" = return "q"
clientSendFunc msg = return $ 'm':msg

getAddress :: IO String
getAddress = do
    args <- getArgs
    return $ if "-a" `elem` args then head $ tail $ dropWhile (/= "-a") args else "127.0.0.1"

getPort :: IO String
getPort = do
    args <- getArgs
    return $ if "-p" `elem` args then head $ tail $ dropWhile (/= "-p") args else "12482"

runClient :: IO ()
runClient = NS.withSocketsDo $ do
        chosenAddr <- getAddress
        chosenPort <- getPort
        addr <- resolve chosenAddr chosenPort
        E.bracket (open addr) NS.close $ \socket -> do 
            Ooey.withOoeyTerminal Ooey.defaultOoeyConfig $ \userIO -> do 
                Ooey.ooeyPutStr userIO $ Right "What is your name?"
                name <- Ooey.ooeyGetLine userIO
                Ooey.ooeyPutStr userIO $ Right $ "Your name is [" ++ name ++  "]"
                sendAll socket $ C.pack $ 'c':(show $ Server.CharacterData name)
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