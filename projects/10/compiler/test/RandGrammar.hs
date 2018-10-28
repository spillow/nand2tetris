module RandGrammar where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)
import Grammar

-- stack ghci compiler:test:compiler-test
-- sample' genKeyword

genKeyword :: Gen Keyword
genKeyword = Keyword <$> keywords
    where keywords = elements [
            "class", "constructor", "function", "method", "field", "static",
            "var", "int", "char", "boolean", "void", "true", "false", "null",
            "this", "let", "do", "if", "else", "while", "return"]

instance Arbitrary Keyword where
    arbitrary = genKeyword

genClassVarType :: Gen ClassVarType
genClassVarType = oneof [return Static, return Field]

instance Arbitrary ClassVarType where
    arbitrary = genClassVarType