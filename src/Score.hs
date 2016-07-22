module Score (module Score, Format (PNG, PDF), Orientation (Portrait, Landscape)) where

import           LilypondProcess
import           Score.Parser
import           Score.Render
import           Score.Types
import           System.Exit
import           Text.Trifecta      (parseFromFile)

render :: Format -> FilePath -> FilePath -> IO ()
render p input outp =
  do scores <- parseFromFile defaultParseScoreFile input
     case scores of
       Nothing -> exitFailure
       Just vs -> writeScorePage p outp vs

writeScorePage :: Format -> FilePath -> (RenderingOptions, [Score]) -> IO ()
writeScorePage format name (o, music) =
     runLilypond format name (printScorePage o music)
