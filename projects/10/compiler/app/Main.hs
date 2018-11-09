module Main where

import System.Environment
import System.Directory
import System.FilePath
import InputHandler

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dir] -> listDirectory dir >>= emit . map (dir </>) . process
        _     -> putStrLn "compiler: <directory of .jack files>"
    where process = filter $ (== ".jack") . takeExtension
