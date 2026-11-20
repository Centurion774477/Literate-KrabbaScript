module Page where

import System.FilePath ( takeFileName )
import Data.Char       ( toUpper )

import Classify        ( LineKind (Code, Prose) )
import Highlight       ( classifyToken )
import Tokenize        ( tokenize )

header_for :: FilePath -> String
header_for path =
  opening ++ map toUpper (takeFileName path) ++ closing
  where
    opening = "\n<h1 style=\"font-family: 'Palatino Linotype', 'Book Antiqua', Palatino, FreeSerif, serif;\">"
    closing = "</h1>"

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
        ,
        ".keyword { color:#d48c0e }"
        ,
        ".type { color:cadetblue }"
        ,
        ".string, .number { color:mediumseagreen }"
      ]

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
