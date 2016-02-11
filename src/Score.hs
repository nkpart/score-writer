module Score (module Score, Format (PNG, PDF), Orientation (Portrait, Landscape)) where

import           LilypondProcess
import           Score.Render
import           Score.Types

writeScorePage :: Orientation -> Format -> FilePath -> [Score] -> IO ()
writeScorePage o format name music =
  do -- writeFile "x.ly" (printScorePage o music)
     runLilypond format name (printScorePage o music)
