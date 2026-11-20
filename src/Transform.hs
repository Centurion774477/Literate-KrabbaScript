module Transform where

import Data.List     ( groupBy )
import Data.Function ( on )

import Classify      ( LineKind (Code, Prose), classify )
import Page          (renderGroup)

-- keep prose and transpile into comments or remove prose entirely
-- adding "without prose" to the end of your command will remove prose, otherwise it will become comments.
-- Reverse will turn normal KrabbaScript into Literate KrabbaScript
data TransformMode = KeepProse | StripProse

data Mode = Transform TransformMode | MakePage | Reverse

unliterate :: String -> String
unliterate = unlines . map convertLine . lines
  where
    convertLine ('#':' ':rest) = rest -- comment becomes prose
    convertLine ('#':rest)     = rest -- handle # without space
    convertLine ""             = ""   -- keep blank lines
    convertLine line           = "    " ++ line  -- indent code

isCode :: LineKind -> Bool
isCode (Code _) = True
isCode _        = False

makePage :: String -> String
makePage = concatMap renderGroup
         . groupBy ((==) `on` isCode)
         . map classify
         . lines

-- transform a classified line into output
transform :: TransformMode -> LineKind -> String
transform KeepProse  (Prose "")   = ""
transform KeepProse  (Prose text) = "# " ++ text
transform StripProse (Prose "")   = ""
transform StripProse (Prose _)    = ""
transform _          (Code code)  = code

process :: Mode -> String -> String
process (Transform mode) = unlines . map (transform mode . classify) . lines
process MakePage         = makePage
process Reverse          = unliterate


