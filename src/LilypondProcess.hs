module LilypondProcess where

import System.Process
import Data.Monoid ((<>))

data Format = PNG | PDF | PS

prettyFormat :: Format -> String
prettyFormat PNG = "png"
prettyFormat PDF = "pdf"
prettyFormat PS = "ps"

runLilypond ::
  Format
  -> FilePath -- name only, extension is added by lilypond
  -> String -- rendered lilypond engraving
  -> IO ()

runLilypond fmt fname music =
  do out <- readCreateProcess (proc "lilypond" ["--" <> prettyFormat fmt, "-o", fname, "-"]) music
     putStrLn out
     return ()
