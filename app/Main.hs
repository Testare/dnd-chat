module Main where

import Lib
import Lib.UI as UI

main :: IO ()
main = UI.createWindowFrame

someFunc :: IO ()
someFunc = do
    putStrLn "Client or Server (s/c)?"
    h <- getLine
    putStrLn $ "You said: (" ++ h ++ ")\n"
    if (h == "c") then runClient else 
        if (h == "s") then runServer else
            UI.runQuitOnQ