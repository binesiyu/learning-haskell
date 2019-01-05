module Prettify
    (Doc
    ,string
    ,text
    ,double
    ) where

data Doc = ToBeDefined
           deriving (Show)

string :: String -> Doc
string str = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined
