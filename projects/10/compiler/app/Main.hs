module Main where

import System.Environment
import Text.Show.Pretty
import Parser

handle (Left msg) = print msg
handle (Right tree) = pPrint tree

emit :: [String] -> IO ()
emit paths = do
    texts <- mapM readFile paths
    let trees = [parseJack file text | (file, text) <- zip paths texts]
    mapM_ handle trees

main = do
    args <- getArgs
    if null args
        then putStrLn "compiler: <path to .jack file>+"
        else emit args
