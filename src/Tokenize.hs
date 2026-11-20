module Tokenize where

import Data.Char ( isAlphaNum, isSpace )

consumeWord :: String -> [String]
consumeWord s =
  let (word, rest) = span (\c -> isAlphaNum c || c == '_') s
  in word : tokenize rest


consumeSpace :: String -> [String]
consumeSpace s =
  let (space, rest) = span isSpace s
  in space : tokenize rest


consumeString :: String -> [String]
consumeString s =
  let (str, rest) = span (/= '"') s
  in case rest of
    ('"':remaining) -> ('"' : str ++ "\"") : tokenize remaining
    _               -> ['"' : str]  -- unterminated string, just return what we have


tokenize :: String -> [String]
tokenize [] = []
tokenize ('#':rest) = ('#':rest) : []  -- rest of line is a comment
tokenize ('"':rest) = consumeString rest
tokenize (c:rest)
  | isAlphaNum c || c == '_' = consumeWord (c:rest)
  | isSpace c                = consumeSpace (c:rest)
  | c == '-', (r:rs) <- rest
  , r == '>'                 = "->" : tokenize rs
  | otherwise                = [c] : tokenize rest
