module Lib.UI 
    ( ooeyTerminal
    , defaultOoeyConfig
    , withOoeyTerminal
    , startOoeyTerminal
    , testOoeyTerminal
    , ooeyPutStr
    , ooeyGetLine
    , ooeyTryPeek
    , ExitMessage
    , OoeyIO
    , OoeyConfig
    , ConsoleMessage
) where
    
import System.Console.Haskeline as HLine
import System.Console.ANSI as ANSI
import GHC.IO.Handle.FD (stdin)
import System.IO
import qualified System.Console.Terminal.Size as TSize (size, Window(..))
import Control.Concurrent
import Control.Concurrent.Async(async, poll, cancel, Async(..))
import Control.Exception(throw)
import Data.Maybe(isJust, fromMaybe)
import Data.Either(isRight)
import qualified Control.Concurrent.STM as STM

{- DATA TYPES -}
{- Should be replaced with ConsoleMessage -}
type ExitMessage = String
type OoeyIO = (STM.TChan (Either ExitMessage String), STM.TChan String)
{- Used for when you need to tell the terminal more than just data -}
data ConsoleMessage = ConsoleMessage 
                        { configUpdate :: Maybe OoeyConfig
                        , shouldQuit :: Bool 
                        , message :: String
                        }  --deriving Show

{- Configuration for the ooey terminal -}
data OoeyConfig = OoeyConfig 
                    { inputSGR :: [ANSI.SGR]
                    , inputPrompt :: String
                    , outputSGR :: [ANSI.SGR]
                    , inputEval :: String -> ConsoleMessage -- Might have to move to a different object
                    }

emptyMessage :: ConsoleMessage
emptyMessage = ConsoleMessage
                    { configUpdate = Nothing
                    , shouldQuit = False
                    , message = cursorUpLineCode 1}

exitMessage :: String -> ConsoleMessage
exitMessage = ConsoleMessage Nothing True 


defaultOoeyConfig :: OoeyConfig
defaultOoeyConfig = OoeyConfig 
                { inputSGR = [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Blue]
                , inputPrompt = "-=-input-=-"
                , outputSGR = [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Red]
                , inputEval = ConsoleMessage Nothing False
                }

{- TEST CODE -}
testing_emptyFromTerminal :: IO (STM.TChan String)
testing_emptyFromTerminal = STM.atomically STM.newTChan
testing_loudToTerminal :: IO (STM.TChan (Either ExitMessage String))
testing_loudToTerminal = STM.atomically $ STM.newTChan >>= \k -> (STM.writeTChan k (Right "This is an example") >> return k)

--Runs the ooeyTerminal, with messages to display read from the first channel and messages
--from the user written to the first channel.
--The input from the user is read from stdin. Possibly in the future we'll support other
--interfaces.
testOoeyTerminal :: IO ()
testOoeyTerminal = do
    m <- testing_loudToTerminal
    n <- testing_emptyFromTerminal
    let config = defaultOoeyConfig
    forkIO $ do
        threadDelay 2000000
        STM.atomically $ STM.writeTChan m $ Right $ (ANSI.cursorUpLineCode 1) ++ (ANSI.setSGRCode [ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Green]) ++ "Blah!"
        threadDelay 20000000
        STM.atomically $ STM.writeTChan m $ Left "Bye!"
    forkIO (echoLoop n m)
    ooeyTerminal config m n
    putStrLn =<< show <$> (STM.atomically $ STM.tryReadTChan n)

{- FUNCTIONS -}

ooeyPutStr :: (STM.TChan (Either ExitMessage String), a) -> Either ExitMessage String -> IO ()
ooeyPutStr (out,_) = STM.atomically .  STM.writeTChan out

ooeyGetLine :: (a, STM.TChan String) -> IO String 
ooeyGetLine = STM.atomically . STM.readTChan . snd

ooeyTryPeek :: (a, STM.TChan String) -> IO (Maybe String)
ooeyTryPeek = STM.atomically . STM.tryPeekTChan . snd

--ooeyTryGetLine :: (a, STM.TChan String) -> IO String 
--ooeyTryGetLine = STM.atomically . flip STM.catchSTM (\(a :: SomeException) -> return "") . STM.readTChan . snd

withOoeyTerminal :: OoeyConfig -> ((STM.TChan (Either ExitMessage String), STM.TChan String) -> IO ()) -> IO ()
withOoeyTerminal config f = do
    innit <- STM.atomically STM.newTChan
    outti <- STM.atomically STM.newTChan
    forkIO $ f (innit,outti)
    ooeyTerminal config innit outti 

startOoeyTerminal :: OoeyConfig -> IO (STM.TChan (Either ExitMessage String), STM.TChan String)
startOoeyTerminal config = do
    innit <- STM.atomically STM.newTChan
    outti <- STM.atomically STM.newTChan
    forkIO $ ooeyTerminal config innit outti 
    return $ (innit, outti)

ooeyTerminal :: OoeyConfig -> STM.TChan (Either ExitMessage String) -> STM.TChan String -> IO () 
ooeyTerminal config toTerminal fromTerminal = do
    oldBuffering <- hGetBuffering stdin 
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    as <- async getChar
    sequence $ putStrLn <$> (replicate 2 "")
    ooeyTerminalLoop "" as config toTerminal fromTerminal
    hSetBuffering stdin oldBuffering
    ANSI.showCursor
    hSetEcho stdin True
    putStrLn ""

ooeyEvalChar :: Char -> String -> String
ooeyEvalChar _ ('\ESC':str) = str
ooeyEvalChar '\ESC' str = '\ESC':'\ESC':str
ooeyEvalChar '\DEL' "" = ""
ooeyEvalChar '\DEL' str = init str
ooeyEvalChar x y = (y ++ (pure x))

--The main loop of the ooeyTerminal. 
ooeyTerminalLoop :: String -> Async Char -> OoeyConfig-> STM.TChan (Either ExitMessage String) -> STM.TChan String -> IO ()
ooeyTerminalLoop currentInput getch config toTerminal fromTerminal = do 
    toOut <- STM.atomically (STM.tryReadTChan toTerminal)
    getchResults <- poll getch
    let (inputChange, nextGetch, nextInput) = flip (maybe (False, (return getch),currentInput)) getchResults $ either throw (\result -> (True, (async getChar), ooeyEvalChar result currentInput))
    let needToPrintInput = (isJust toOut) || inputChange
    quitSignalFromOutput <- maybe (return False) printOutput toOut 
    nextInput2 <- evalInput nextInput  
    if inputChange then repositionCursor else return ()
    if needToPrintInput then printInput (fromMaybe "" nextInput2) else return ()
    --Gather new input
    --let updatedInput
    --ANSI.showCursor
    realNextGetch <- nextGetch
    if quitSignalFromOutput 
        then (closeTerminal realNextGetch) 
        else (maybe closeTerminal recur nextInput2) realNextGetch
    where 
        repositionCursor :: IO ()
        repositionCursor = do
            (Just TSize.Window{TSize.width=w}) <- TSize.size
            let inputBufferSize = succ $ (pred (length currentInput)) `div` w
            ANSI.hideCursor
            cursorUpLine inputBufferSize
        recur :: String -> Async Char -> IO ()
        recur input_ getch_ = do
            yield
            threadDelay 2500
            ooeyTerminalLoop input_ getch_ config toTerminal fromTerminal
        printInput :: String -> IO ()
        printInput input_ = do
            ANSI.hideCursor
            ANSI.setCursorColumn 0
            putStr $ inputPrompt config
            ANSI.clearFromCursorToLineEnd
            putStrLn ""
            ANSI.setSGR $ inputSGR config
            putStr input_
            ANSI.setSGR $ [ANSI.Reset]
            ANSI.clearFromCursorToLineEnd
            putStrLn $ ANSI.cursorUpLineCode 1
            --ANSI.showCursor
        printOutput :: (Either String String) -> IO Bool
        printOutput output_ = do
            let (quitSignal, outputString) = either (\x->(True, x)) (\x->(False, x)) output_
            repositionCursor
            ANSI.setSGR $ outputSGR config
            putStrLn $ ANSI.clearLineCode ++ outputString
            ANSI.setSGR $ [ANSI.Reset]
            return quitSignal
        closeTerminal :: Async Char -> IO ()
        closeTerminal getch_ = (cancel getch_)
        --Checks if the input should be written to from terminal, and what the 
        --new input buffer will be in either case
        evalInput :: String -> IO (Maybe String)
        evalInput input_ = do
            if input_ == "\n" then return (Just "")
                else if input_ /= "" && (last input_) == '\n' 
                    then STM.atomically (STM.writeTChan fromTerminal (init input_)) >> ANSI.showCursor >> return (Just "") 
                    else return (Just input_)

echoLoop :: STM.TChan String -> STM.TChan (Either ExitMessage String) -> IO ()
echoLoop in_ out_ = do
    cont <- STM.atomically $ do
        k <- STM.readTChan in_
        let echoValue = if (k == "quit") then Left "Closing..." else Right ("You said:" ++ k)
        STM.writeTChan out_ echoValue
        return $ isRight echoValue
    if cont then (echoLoop in_ out_) else return ()
