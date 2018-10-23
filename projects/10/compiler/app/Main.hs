module Main where

import System.Environment
import Grammar
import Lib

parse :: String -> Class
parse text = Class (Identifier "myclass") [] []

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "compiler: <path to .jack file>+"
        else emit args
    return ()
    where
        emit :: [String] -> IO ()
        emit paths = do
            texts <- mapM readFile paths
            let trees = map parse texts
            mapM_ print trees
            return ()
