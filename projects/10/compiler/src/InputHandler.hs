module InputHandler(emit) where

import Text.Show.Pretty
import Parser

handle :: (Show a, Show b) => Either a b -> IO Bool
handle (Left msg) = print msg >> return False
handle (Right tree) = pPrint tree >> return True

emit :: [String] -> IO [Bool]
emit paths = do
    texts <- mapM readFile paths
    let trees = [parseJack file text | (file, text) <- zip paths texts]
    mapM handle trees