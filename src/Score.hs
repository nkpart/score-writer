module Score (module Score, Format (PNG, PDF), Orientation (Portrait, Landscape)) where

import           LilypondProcess
import           Score.Render
import           Score.Types

writeScore :: Orientation -> Format -> FilePath -> Score -> IO ()
writeScore o fmt name music = runLilypond fmt name (printScorePage o [music])

writeScorePage :: Orientation -> Format -> FilePath -> [Score] -> IO ()
writeScorePage o format name music = runLilypond format name (printScorePage o music)
