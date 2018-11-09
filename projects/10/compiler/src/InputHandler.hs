module InputHandler(emit) where

--import Text.Show.Pretty
import Parser
import qualified Grammar as G
import CodeGen
import System.IO
import System.FilePath

writeProgram :: FilePath -> Program -> IO ()
writeProgram path prog = do
    h <- openFile path WriteMode
    mapM_ (hPrint h) prog
    hClose h

handle :: Show a => (FilePath, Either a G.Class) -> IO ()
handle (_, Left msg) = print msg
handle (path, Right tree) = case codegen tree of
    Left msg      -> print msg
    Right program -> writeProgram vmPath program
    where vmPath = replaceExtension path "vm"

emit :: [String] -> IO ()
emit paths = do
    texts <- mapM readFile paths
    let trees = [(file, parseJack file text) | (file, text) <- zip paths texts]
    mapM_ handle trees