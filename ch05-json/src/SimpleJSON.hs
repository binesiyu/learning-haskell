module SimpleJSON
    ( JValue(..)
     ,renderJValueSimple
     ,putJValue
    ) where

import Data.List (intercalate)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

renderJValueSimple :: JValue -> String
renderJValueSimple (JString s)   = show s
renderJValueSimple (JNumber n)   = show n
renderJValueSimple (JBool True)  = "true"
renderJValueSimple (JBool False) = "false"
renderJValueSimple JNull         = "null"

renderJValueSimple (JObject o) = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs ps = intercalate ", " (map renderPair ps)
        renderPair (k,v)   = show k ++ ": " ++ renderJValueSimple v

renderJValueSimple (JArray a) = "[" ++ values a ++ "]"
  where values [] = ""
        values vs = intercalate ", " (map renderJValueSimple vs)

putJValue :: JValue -> IO ()
putJValue = putStrLn . renderJValueSimple
