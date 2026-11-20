module Classify where

import Data.List ( isPrefixOf )

-- a line is either prose text or indented code
data LineKind = Prose String | Code String

-- classify a line into code or prose
classify :: String -> LineKind
classify line
  | "    " `isPrefixOf` line = Code (drop 4 line) -- indented line
  | "\t"   `isPrefixOf` line = Code (drop 1 line) -- indented line as well
  | otherwise                = Prose line -- anything else is prose
