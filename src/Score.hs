module Score where

import           LilypondProcess
import           Score.Render
import           Score.Types

writeScorePNG :: FilePath -> Score -> IO ()
writeScorePNG name music = runLilypond PNG name (printScore music)

writeScorePDF :: FilePath -> Score -> IO ()
writeScorePDF name music = runLilypond PDF name (printScore music)

writeScoreBookPDF :: FilePath -> [[Score]] -> IO ()
writeScoreBookPDF name music = runLilypond PDF name (printScoreBook music)
