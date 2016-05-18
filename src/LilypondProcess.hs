module LilypondProcess where

import System.Process
import Data.Monoid ((<>))
import System.Directory
import System.IO.Temp
import System.IO

data Format = PNG | PDF | PS

runLilypond :: Format -- target format
            -> FilePath -- where to write the score to
            -> String -- rendered lilypond engraving
            -> IO ()
runLilypond fmt fname music =
  withSystemTempFile "score" $ \tmpFp h -> do
       hClose h
       out <- readCreateProcess (proc "lilypond" ["--" <> prettyFormat fmt, "-o", tmpFp, "-"]) music
       putStrLn out
       -- Lilypond adds an extension to the output file path
       copyFile (tmpFp <> "." <> prettyFormat fmt) fname
       return ()

prettyFormat :: Format -> String
prettyFormat PNG = "png"
prettyFormat PDF = "pdf"
prettyFormat PS = "ps"

test :: IO ()
test =
  do runLilypond PNG "x.png" "{ c' }"
     print =<< doesFileExist "x.png"
     -- removeFile "x.png"

