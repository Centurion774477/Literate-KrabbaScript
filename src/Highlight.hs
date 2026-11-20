module Highlight where

import Data.Char ( isDigit )

isStringLiteral :: String -> Bool
isStringLiteral ('"':rest) = last rest == '"'
isStringLiteral _          = False

isNumberLiteral :: String -> Bool
isNumberLiteral = all isDigit

classifyToken :: [Char] -> [Char]
classifyToken token
  | head token == '#'      = "<span class=\"comment\">" ++ token ++ "</span>"
  | token `elem` keywords  = "<span class=\"keyword\">" ++ token ++ "</span>"
  | token `elem` types     = "<span class=\"type\">"    ++ token ++ "</span>"
  | isNumberLiteral token  = "<span class=\"number\">"  ++ token ++ "</span>"
  | isStringLiteral token  = "<span class=\"string\">"  ++ token ++ "</span>"
  | otherwise              = token
  where
    keywords = ["mod", "var", "val", "func", "when", "do", "return", "break", "else", "if", "struct"]
    types    = ["Str", "I32", "I64", "F32", "F64", "Bool"]
