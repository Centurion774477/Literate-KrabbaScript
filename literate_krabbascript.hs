
-- Copyright Reece Turner 2026

module Main where

import System.Environment ( getArgs )
import Data.List ( isPrefixOf )
import System.FilePath ( replaceExtension, takeExtension, takeFileName )
import Data.Char ( toUpper, isDigit, isAlphaNum, isSpace )
import System.Directory ( doesFileExist ) 
import System.Exit ( exitFailure )
import Data.List ( groupBy )
import Data.Function ( on )

-- a line is either prose text or indented code
data LineKind = Prose String | Code String

-- keep prose and transpile into comments or remove prose entirely
-- adding "without prose" to the end of your command will remove prose, otherwise it will become comments.
-- Reverse will turn normal KrabbaScript into Literate KrabbaScript
data Mode = KeepProse | StripProse | MakePage | Reverse

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


isCode :: LineKind -> Bool
isCode (Code _) = True
isCode _        = False

isStringLiteral :: String -> Bool
isStringLiteral ('"':rest) = last rest == '"'
isStringLiteral _          = False

isNumberLiteral :: String -> Bool
isNumberLiteral = all isDigit


classifyToken token
  | head token == '#'          = "<span class=\"comment\">" ++ token ++ "</span>"
  | token `elem` keywords      = "<span class=\"keyword\">" ++ token ++ "</span>"
  | token `elem` types         = "<span class=\"type\">"    ++ token ++ "</span>"
  | isNumberLiteral token      = "<span class=\"number\">"  ++ token ++ "</span>"
  | isStringLiteral token      = "<span class=\"string\">"  ++ token ++ "</span>"
  | otherwise                  = token
  where
    keywords = ["mod", "var", "val", "func", "when", "do", "return", "break", "else", "if", "struct"]
    types    = ["Str", "I32", "I64", "F32", "F64", "Bool"]


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


-- group consecutive lines of the same kind, then render each group
renderGroup :: [LineKind] -> String
renderGroup [] = ""
renderGroup group@(Code _ : _) =
  "<pre><code>" ++ concatMap highlightLine (map (\(Code c) -> c) group) ++ "</code></pre>"
  where
    highlightLine line = concatMap classifyToken (tokenize line) ++ "\n"
renderGroup group =
  concatMap renderProse group
  where
    renderProse (Prose "")   = ""
    renderProse (Prose text) = "<p>" ++ text ++ "</p>\n"
    renderProse _            = ""


makePage :: String -> String
makePage = concatMap renderGroup
         . groupBy ((==) `on` isCode)
         . map classify
         . lines

unliterate :: String -> String
unliterate = unlines . map convertLine . lines
  where
    convertLine ('#':' ':rest) = rest -- comment becomes prose
    convertLine ('#':rest)     = rest -- handle # without space
    convertLine ""             = ""   -- keep blank lines
    convertLine line           = "    " ++ line  -- indent code


-- process file
process :: Mode -> String -> String
process KeepProse  = unlines . map (transform KeepProse  . classify) . lines
process StripProse = unlines . map (transform StripProse . classify) . lines
process MakePage   = makePage
process Reverse    = unliterate

sandwich :: String -> String -> String
sandwich title text =
  header ++ text ++ footer
  where
    header = "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"UTF-8\">\
             \<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\
             \<title>" ++ title ++ "</title>\
             \<style>" ++ style ++ "</style>\
             \</head><body>"
    footer = "<h5>Created with <a href=\"https://github.com/Centurion774477/Literate-KrabbaScript\">Literate KrabbaScript</a></h5></body></html>"
    style = unlines
      [ "body {"
        ,
        "font-family: 'Palatino Linotype', 'Book Antiqua', Palatino, FreeSerif, serif; font-size: 15px; line-height: 22px;"
        ,
        "color: #252519; background-color: #f5f5ff; margin: 0; padding: 0; min-height: 100%;"
        ,
        "}"
        ,
        "a { color: #261a3b; }"
        ,
        "a:visited { color: #261a3b; }"
        ,
        "p, ul, ol { margin: 0 0 15px; }"
        ,
        "h1, h2, h3, h4, h5, h6 { margin: 30px 0 15px; color: inherit; }"
        ,
        "h1 { margin-top: 40px; }"
        ,
        "hr { border: 0; border-top: 1px solid #e5e5ee; height: 1px; margin: 20px 0; }"
        ,
        "pre, tt, code {"
        ,
        "font-size: 12px; line-height: 18px; font-family: Menlo, Monaco, Consolas, \"Lucida Console\", monospace;"
        ,
        "margin: 0; padding: 0; background-color: #c8dff6;"
        ,
        "}"
        ,
        "body { max-width: 400px; margin: 0 auto; padding: 0 16px; }"
      ]


-- run: runghc literate_krabbascript.hs test.litkrab test.ks
-- test page creation: runghc literate_krabbascript.hs test.litkrab test.html make page

header_for :: FilePath -> String
header_for path =
  "\n<h1 style=\"font-family: 'Palatino Linotype', 'Book Antiqua', Palatino, FreeSerif, serif;\">" ++ map toUpper (takeFileName path) ++ "</h1>"

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
            let title = takeFileName inputFile in
              writeFile outputFile (sandwich title (header_for inputFile ++ process MakePage contents))

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
            let title = takeFileName inputFile in
              writeFile outputFile (sandwich title (header_for inputFile ++ process MakePage contents))
        "in reverse" -> do
          contents <- readFile inputFile
          
          let outputFile = replaceExtension inputFile ".suomi" in
            writeFile outputFile (process Reverse contents)


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

