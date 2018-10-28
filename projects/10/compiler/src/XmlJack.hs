{-# LANGUAGE OverloadedStrings #-}

module XmlJack where

import Data.Text (Text, pack)
import Text.XML.Writer
import Text.XML
import Grammar

-- xx = renderText (def { rsPretty = Prelude.True }) doc

mytest = document "tokens" $ do
    toXML (Keyword "class")
    toXML (Symbol ';')
    toXML (IntegerConstant 17)
    toXML (StringConstant "stuff")
    toXML (Identifier "myvar")

instance ToXML Keyword where
    toXML (Keyword x) = element "keyword" $ content (pack x)

instance ToXML Symbol where
    toXML (Symbol x) = element "symbol" $ content (pack [x])

instance ToXML IntegerConstant where
    toXML (IntegerConstant x) = element "integerConstant" $ content (pack $ show x)

instance ToXML StringConstant where
    toXML (StringConstant x) = element "stringConstant" $ content (pack x)

instance ToXML Identifier where
    toXML (Identifier x) = element "identifier" $ content (pack x)

instance ToXML ClassVarType where
    toXML Static = element "keyword" $ content (pack "static")
    toXML Field  = element "keyword" $ content (pack "field")

instance ToXML SubroutineVariety where
    toXML Constructor = element "keyword" $ content (pack "constructor")
    toXML Function    = element "keyword" $ content (pack "function")
    toXML Method      = element "keyword" $ content (pack "method")

instance ToXML Type where
    toXML Int          = element "keyword" $ content (pack "int")
    toXML Char         = element "keyword" $ content (pack "char")
    toXML Boolean      = element "keyword" $ content (pack "boolean")
    toXML (TypeName c) = element "identifier" $ content (pack $ getIdentifier c)

instance ToXML SubroutineType where
    toXML Void       = element "keyword" $ content (pack "void")
    toXML (SubTy ty) = toXML ty