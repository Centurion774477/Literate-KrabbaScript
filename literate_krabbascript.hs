
module Main where

import System.Environment (getArgs)
import Data.List (isPrefixOf)
import System.FilePath (replaceExtension)

-- a line is either prose text or indented code
data LineKind = Prose String | Code String

-- classify a single line
classify :: String -> LineKind
classify line
  | "    " `isPrefixOf` line = Code (drop 4 line) -- indented line
  | "\t"   `isPrefixOf` line = Code (drop 1 line) -- indented line as well
  | otherwise                = Prose line

-- Transform a classified line into output
transform :: LineKind -> String
transform (Prose "") = ""         -- keep blank lines blank
transform (Prose text) = "# " ++ text -- add a comment for prose lines
transform (Code code)  = code

-- process file
process :: String -> String
process = unlines . map (transform . classify) . lines

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do
      contents <- readFile inputFile
      writeFile outputFile (process contents)
    [inputFile] -> do
        contents <- readFile inputFile
        let outputFile = replaceExtension inputFile ".ks"
        writeFile outputFile (process contents)
    _ -> putStrLn "Usage: literate_krabbascript <input> [output]"