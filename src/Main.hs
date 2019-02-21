module Main where

import Server
import Client
import System.Environment

main :: IO ()
main = do
    getArgs >>= \case
        ["sv"] -> do
            putStrLn "sv start"
            runServer "8000"
            putStrLn "sv end"
            
        ["cl"] -> do
            putStrLn "cl start"
            runClient "127.0.0.1" "8000"
            putStrLn "cl end"
        _ -> do
            putStrLn "usage: chaths <sv | cl>"
