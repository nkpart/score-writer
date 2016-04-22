module Score (module Score, Format (PNG, PDF), Orientation (Portrait, Landscape)) where

import           Control.Monad.Cont
import           Data.Monoid        ((<>))
import           Score.Render
import           Score.Types
import           Score.Parser
import           Text.Trifecta (parseFromFile)
import           System.Exit
import           System.IO
import           System.IO.Temp
import           System.Process

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
           return $ fp <> ".pdf"
      liftIO $ collatePDFs scores destFile
       where withSystemTempFile' = ContT $ \f -> withSystemTempFile "score-writer" (\fp h -> hClose h >> f fp)

writeScorePage :: Orientation -> Format -> FilePath -> [Score] -> IO ()
writeScorePage o format name music =
  -- do writeFile "x.ly" (printScorePage o music)
     runLilypond format name (printScorePage o music)

collatePDFs :: [FilePath] -> FilePath -> IO ()
collatePDFs pdfs outFile =
   callProcess "/usr/local/bin/gs" (["-dBATCH", "-dNOPAUSE", "-q", "-sDEVICE=pdfwrite", "-sOutputFile="<>outFile] ++ pdfs)

data Format = PNG | PDF | PS

runLilypond ::
   Format
   -> FilePath -- name only, extension is added by lilypond
   -> String -- rendered lilypond engraving
   -> IO ()

runLilypond fmt fname music =
   do out <- readCreateProcess (proc "lilypond" ["--" <> prettyFormat fmt, "-o", fname, "-"]) music
      putStrLn out
      return ()

prettyFormat :: Format -> String
prettyFormat PNG = "png"
prettyFormat PDF = "pdf"
prettyFormat PS = "ps"

