module Lib.UI where
    
import System.Console.Haskeline as HLine
import System.Console.ANSI as ANSI
import GHC.IO.Handle.FD (stdin)
import System.IO
import qualified System.Console.Terminal.Size as TSize (size, Window(..))
import Control.Concurrent
import Control.Concurrent.Async(async, poll, cancel, Async(..))
import Control.Exception(throw)

--createOoeyTerminal :: MVar (Either String String) -> MVar String -> IO ()




testing_emptyFromTerminal :: IO (MVar String)
testing_emptyFromTerminal = newEmptyMVar 
testing_emptyToTerminal :: IO (MVar (Either String String))
testing_emptyToTerminal = newEmptyMVar 
testing_loudToTerminal :: IO (MVar (Either String String))
testing_loudToTerminal = newEmptyMVar >>= \k -> (putMVar k (Right "This is an example.") >> return k)
testing_quitToTerminal :: IO (MVar (Either String String))
testing_quitToTerminal = newEmptyMVar >>= \k -> (putMVar k (Left "This is an example.") >> return k)

ooeyTerminalInit :: MVar (Either String String) -> MVar String -> IO () 
ooeyTerminalInit toTerminal fromTerminal = do
    hSetEcho stdin False
    as <- async getChar
    sequence $ putStrLn <$> (replicate 2 "")
    ooeyTerminalLoop "" as toTerminal fromTerminal
    hSetEcho stdin True

ooeyTerminalLoop :: String -> Async Char -> MVar (Either String String) -> MVar String -> IO ()
ooeyTerminalLoop currentInput getch toTerminal fromTerminal = do 
    toOut <- tryTakeMVar toTerminal
    getchResults <- poll getch
    let (nextGetch,nextInput) = flip (maybe ((return getch),currentInput)) getchResults $ either throw (\result -> ((async getChar), if (result /= '\DEL') then (currentInput ++ (pure result)) else (if currentInput /= "" then (init currentInput) else "")))
    --Print out new output
    (Just TSize.Window{TSize.width=w}) <- TSize.size
    let inputBufferSize = succ $ (pred (length currentInput)) `div` w
    ANSI.hideCursor
    cursorUp inputBufferSize
    maybe (return ()) (either (putStrLn . (ANSI.clearLineCode ++)) (putStrLn . (ANSI.clearLineCode ++))) toOut 
    ANSI.setCursorColumn 0
    putStr "---"
    ANSI.clearFromCursorToLineEnd
    putStrLn ""
    --Gather new input
    --let updatedInput
    realNextGetch <- nextGetch

    if nextInput /= "" && (last nextInput) == '\n' then (cancel realNextGetch) >> putStrLn (init nextInput) >> ANSI.showCursor else do
        putStr nextInput
        ANSI.showCursor
        ANSI.clearFromCursorToLineEnd
        recur nextInput realNextGetch
    where 
        recur input_ getch_ = do
            yield
            threadDelay 25
            ooeyTerminalLoop input_ getch_ toTerminal fromTerminal
        printInput input_ = do
            ANSI.hideCursor
            ANSI.clearLine
            putStrLn "---"
            ANSI.showCursor
        printOutput output_ input_ = do
            putStrLn ""



testOoeyTerminal :: IO ()
testOoeyTerminal = do
    m <- testing_loudToTerminal
    n <- testing_emptyFromTerminal
    ooeyTerminalInit m n

--For fun
createWindowFrame :: IO ()
createWindowFrame = do
    (Just TSize.Window{TSize.height=h,TSize.width=w}) <- TSize.size
    putStrLn $ (show h) ++ "," ++ (show w)
    ANSI.setCursorPosition 0 0
    putStrLn $ (replicate w '-')
    let frameSides = ('|':(replicate (w - 2) ' ') ++ "|")
    sequence $ (replicate (h - 2) $ (putStrLn frameSides))
    putStr $ (replicate w '-')
    --ANSI.scrollPageDown 1
    ANSI.cursorUp 2
    putStrLn ""

runQuitOnQ :: IO () 
--runQuitOnQ = HLine.runInputTBehavior (HLine.useFileHandle stdin) HLine.defaultSettings quitOnQ
runQuitOnQ = HLine.runInputT HLine.defaultSettings quitOnQ

quitOnQ :: InputT IO ()
quitOnQ = do
    inputCharacter <- HLine.getInputChar ""
    case inputCharacter of
        Nothing -> outputStr "!"
        Just 'q' -> return ()
        Just inpt -> do outputStr $ (ANSI.cursorUpCode 1) 
                        quitOnQ

withoutEcho :: IO () -> IO ()
withoutEcho a = (hSetEcho stdin False) >> a >> (hSetEcho stdin True)

easyPassword :: String -> IO ()
easyPassword str = do
    k <- getChar
    if (k == '\n') then putStrLn (str ++ [k]) else easyPassword (str ++ [k])

getMaybeChar :: IO (Maybe Char)
getMaybeChar = HLine.runInputT HLine.defaultSettings $ HLine.getInputChar (ANSI.cursorUpCode 1)

charReadLoop :: IO String
charReadLoop = do
    maybeChar <- getMaybeChar
    case maybeChar of
        Nothing -> return ""
        Just '.' -> return "."
        Just input -> charReadLoop >>= return . ((:) input)



