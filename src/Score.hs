module Score (module Score, Format (PNG, PDF), Orientation (Portrait, Landscape)) where

import           Control.Monad.Cont
import           Data.Monoid        ((<>))
import           LilypondProcess
import           Score.Parser
import           Score.Render
import           Score.Types
import           System.Exit
import           System.IO
import           System.IO.Temp
import           System.Process
import           Text.Trifecta      (parseFromFile)

render :: Format -> Orientation -> FilePath -> FilePath -> IO ()
render p orientation inp outp =
  do x <- parseFromFile defaultParseScore inp
     case x of
       Nothing -> exitFailure
       Just score -> writeScorePage orientation p outp [score]

assembleSet :: FilePath -> [(Orientation, [Score])] -> IO ()
assembleSet destFile things =
  flip runContT return $
   do scores <- forM things $ \(o, scores) ->
        do fp <- withSystemTempFile'
           liftIO $ writeScorePage o PDF fp scores
           return fp
      liftIO $ collatePDFs scores destFile
       where withSystemTempFile' = ContT $ \f -> withSystemTempFile "score-writer" (\fp h -> hClose h >> f fp)

writeScorePage :: Orientation -> Format -> FilePath -> [Score] -> IO ()
writeScorePage o format name music =
     runLilypond format name (printScorePage o music)

collatePDFs :: [FilePath] -> FilePath -> IO ()
collatePDFs pdfs outFile =
   callProcess "/usr/local/bin/gs" (["-dBATCH", "-dNOPAUSE", "-q", "-sDEVICE=pdfwrite", "-sOutputFile="<>outFile] ++ pdfs)

