module InputHandler(emit) where

--import Text.Show.Pretty
import Parser
import qualified Grammar as G
import CodeGen

handle :: Show a => Either a G.Class -> IO Bool
handle (Left msg) = print msg >> return False
handle (Right tree) = case codegen tree of
    Left msg      -> print msg >> return False
    Right program -> mapM_ print program >> return True

emit :: [String] -> IO [Bool]
emit paths = do
    texts <- mapM readFile paths
    let trees = [parseJack file text | (file, text) <- zip paths texts]
    mapM handle trees