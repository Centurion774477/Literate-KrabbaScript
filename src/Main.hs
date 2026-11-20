module Main where

import System.Environment ( getArgs )
import System.FilePath    ( replaceExtension, takeExtension, takeFileName )
import System.Directory   ( doesFileExist ) 
import System.Exit        ( exitFailure )

import Transform          ( process
                          ,
                          TransformMode( StripProse, KeepProse )
                          ,
                          Mode(..)
                          )
import Page               ( sandwich, header_for )


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
      putStrLn usage
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
          writeFile outputFile $ process (Transform StripProse) contents
        "make page" -> do
          if takeExtension outputFile /= ".html"
          then putStrLn "Your output file has to be a .html file if you're generating a page."
          else do
            contents <- readFile inputFile
            let title = takeFileName inputFile in
              writeFile outputFile $ sandwich title $ header_for inputFile ++ process MakePage contents
        _ -> 
          putStrLn "Unknown flags." >> exitFailure

    -- omit output file and passes either 'without prose' or 'make page'
    [inputFile, flag1, flag2] -> do
      case flag1 ++ " " ++ flag2 of
        "without prose" -> do
          contents <- readFile inputFile
        
          let outputFile = replaceExtension inputFile ".ks" in
            writeFile outputFile $ process (Transform StripProse) contents
        "make page" -> do
          contents <- readFile inputFile

          let outputFile = replaceExtension inputFile ".html" in
            let title = takeFileName inputFile in
              writeFile outputFile $ sandwich title $ header_for inputFile ++ process MakePage contents
        "in reverse" -> do
          contents <- readFile inputFile
          
          let outputFile = replaceExtension inputFile ".suomi" in
            writeFile outputFile $ process Reverse contents
        _ ->
          putStrLn "Unknown flags." >> exitFailure


    -- normal case — an input and output is provided; transpile normally.
    [inputFile, outputFile] -> do
      contents <- readFile inputFile
      writeFile outputFile $ process (Transform KeepProse) contents

    -- infer output file from input file
    [inputFile] -> do
        contents <- readFile inputFile
        let outputFile = replaceExtension inputFile ".ks" in
          writeFile outputFile $ process (Transform KeepProse) contents

    -- invalid command or no command at all
    _ -> do
      putStrLn usage
  where
    usage = unlines
      [ "Usage: literate_krabbascript <input> [output]"
      ,
      "To filter out prose, use the 'without prose' flag."
      ,
      "To generate a .html page, use the 'make page' flag."
      ,
      "To turn a .ks file into a Literate file, use the 'in reverse' flag."
      ]
