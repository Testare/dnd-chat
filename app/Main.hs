module Main where

import Lib
import Lib.UI as UI

main :: IO ()
main = someFunc

someFunc :: IO ()
someFunc = do
    putStrLn "Client or Server (s/c)?"
    h <- getLine
    putStrLn $ "You said: (" ++ h ++ ")"
    if (h == "c") then runClient else 
        if (h == "s") then runServer else 
            if (h == "ooey") then UI.testOoeyTerminal else 
            someFunc