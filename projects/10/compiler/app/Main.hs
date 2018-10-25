module Main where

import System.Environment
import Parser

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
            let trees = map myparse texts
            mapM_ print trees
            return ()
