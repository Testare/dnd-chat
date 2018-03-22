module Main where

import Lib

main :: IO ()
main = someFunc

someFunc :: IO ()
someFunc = do
    putStrLn "Client or Server (s/c)?"
    h <- getLine
    putStrLn $ "You said: (" ++ h ++ ")\n"
    if (h == "c") then runClient else runServer