
module Main where

import System.Environment (getArgs)
import Data.List (isPrefixOf)
import System.FilePath (replaceExtension)

-- a line is either prose text or indented code
data LineKind = Prose String | Code String

-- keep prose and transpile into comments or remove prose entirely
-- adding "without prose" to the end of your command will remove prose, otherwise it will become comments.
data Mode = KeepProse | StripProse

-- classify a single line
classify :: String -> LineKind
classify line
  | "    " `isPrefixOf` line = Code (drop 4 line) -- indented line
  | "\t"   `isPrefixOf` line = Code (drop 1 line) -- indented line as well
  | otherwise                = Prose line

-- transform a classified line into output
transform :: Mode -> LineKind -> String
transform KeepProse (Prose "") = "" -- keep blank lines blank
transform KeepProse (Prose text) = "# " ++ text -- add a comment for prose lines
transform StripProse (Prose "") = "" -- remove blank lines
transform StripProse (Prose text) = "" -- remove prose lines
transform _ (Code code)  = code

-- process file
process :: Mode -> String -> String
process mode = unlines . map (transform mode . classify) . lines

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- remove prose
    [inputFile, outputFile, "without", "prose"] -> do
        contents <- readFile inputFile
        writeFile outputFile (process StripProse contents)

    -- infer output and remove prose
    [inputFile, "without", "prose"] -> do
        contents <- readFile inputFile
        let outputFile = replaceExtension inputFile ".ks"
        writeFile outputFile (process StripProse contents)

    [inputFile, outputFile] -> do
      contents <- readFile inputFile
      writeFile outputFile (process KeepProse contents)

    -- infer output file from input file
    [inputFile] -> do
        contents <- readFile inputFile
        let outputFile = replaceExtension inputFile ".ks"
        writeFile outputFile (process KeepProse contents)
    _ -> putStrLn "Usage: literate_krabbascript <input> [output]"