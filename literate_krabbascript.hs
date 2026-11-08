
-- Copyright Reece Turner 2026

module Main where

import System.Environment (getArgs)
import Data.List (isPrefixOf, singleton)
import System.FilePath (replaceExtension)
import System.FilePath (takeExtension)
import System.FilePath (takeFileName)
import Data.Char (toUpper)
import System.Directory 
import System.Exit

-- a line is either prose text or indented code
data LineKind = Prose String | Code String

-- keep prose and transpile into comments or remove prose entirely
-- adding "without prose" to the end of your command will remove prose, otherwise it will become comments.
data Mode = KeepProse | StripProse | MakePage

-- classify a line into code or prose
classify :: String -> LineKind
classify line
  | "    " `isPrefixOf` line = Code (drop 4 line) -- indented line
  | "\t"   `isPrefixOf` line = Code (drop 1 line) -- indented line as well
  | otherwise                = Prose line -- anything else is prose


-- transform a classified line into output
transform :: Mode -> LineKind -> String
transform KeepProse (Prose "")    = "" -- keep blank lines blank
transform KeepProse (Prose text)  = "# " ++ text -- add a comment for prose lines

transform StripProse (Prose "")   = "" -- remove blank lines
transform StripProse (Prose text) = "" -- remove prose lines

transform MakePage (Prose "")     = "" -- remove blank lines
transform MakePage (Prose text)   = "<p>" ++ text ++ "</p>" -- wrap prose in HTML paragraph tags
transform MakePage (Code code)    = "<pre><code>" ++ code ++ "</code></pre>" -- wrap code in HTML pre/code tags

transform _ (Code code)           = code


-- process file
process :: Mode -> String -> String
process mode = unlines . map (transform mode . classify) . lines

sandwich text =
  header <> text <> footer
  where
    header = "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"UTF-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\"><meta http-equiv=\"X-UA-Compatible\" content=\"ie=edge\"><title>Literate KrabbaScript</title></head><body>"
    footer = "<h5>Created with Literate KrabbaScript</h5></body></html>"
    -- todo: add a link to the GitHub repo in the footer

-- run: runghc literate_krabbascript.hs test.litkrab test.ks
-- test page creation: runghc literate_krabbascript.hs test.litkrab test.html make page

header_for path =
  "\n<h1 style=\"font-family: 'Palatino Linotype', 'Book Antiqua', Palatino, FreeSerif, serif;\">" <> map toUpper (takeFileName path) <> "</h1>"

main :: IO ()
main = do
  args <- getArgs

  -- check if no args were passed or if a file was passed and it doesn't exist
  case args of
    (inputFile:_) -> do
      exists <- doesFileExist inputFile
      if not exists
        then putStrLn "Your input file does not exist." >> exitFailure
        else return ()
    [] -> do
      putStrLn "Usage: literate_krabbascript <input> [output]"
      putStrLn "To filter out prose, add 'without prose' to your command."
      putStrLn "To generate a .html page, add 'make page' to your command."
      exitFailure

  case args of
    -- handles both:
    -- `input output without prose`
    -- and
    -- `input output make page`
    [inputFile, outputFile, flag1, flag2] -> do
      case flag1 ++ " " ++ flag2 of
        "without prose" -> do
          contents <- readFile inputFile
          writeFile outputFile (process StripProse contents)
        "make page" -> do
          if takeExtension outputFile /= ".html"
          then putStrLn "Your output file has to be a .html file if you're generating a page."
          else do
            contents <- readFile inputFile
            writeFile outputFile (sandwich (header_for inputFile <> process MakePage contents))

    -- omit output file and passes either 'without prose' or 'make page'
    [inputFile, flag1, flag2] -> do
      case flag1 ++ " " ++ flag2 of
        "without prose" -> do
          contents <- readFile inputFile
        
          let outputFile = replaceExtension inputFile ".ks" in
            writeFile outputFile (process StripProse contents)
        "make page" -> do
          contents <- readFile inputFile

          let outputFile = replaceExtension inputFile ".html" in
            writeFile outputFile (sandwich (header_for inputFile <> process MakePage contents))

    -- normal case — an input and output is provided; transpile normally.
    [inputFile, outputFile] -> do
      contents <- readFile inputFile
      writeFile outputFile (process KeepProse contents)

    -- infer output file from input file
    [inputFile] -> do
        contents <- readFile inputFile
        let outputFile = replaceExtension inputFile ".ks" in
          writeFile outputFile (process KeepProse contents)

    _ -> do
      putStrLn "Usage: literate_krabbascript <input> [output]"
      putStrLn "To filter out prose, add 'without prose' to your command."
      putStrLn "To generate a .html page, add 'make page' to your command."

