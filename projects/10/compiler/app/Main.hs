module Main where

import System.Environment
import InputHandler
import Control.Monad (void)

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "compiler: <path to .jack file>+"
        else void $ emit args
