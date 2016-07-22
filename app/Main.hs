module Main where

-- import           Control.Monad
import           LilypondProcess
import           Options.Applicative
import           Score
-- import           Data.Time
-- import           System.Directory
-- import           System.FilePath
-- import           System.Process

data CLI
  = Render FilePath
           String
  -- | View FilePath

cli :: Parser CLI
cli =
  (subparser . mconcat)
    [
     command "render"
             (info (Render <$>
                    strOption (long "score-file") <*>
                    strOption (long "output-file"))
              (progDesc "Render a .score file")
             )
    -- ,command "view" (info (View <$> strOption (long "score-file")) (progDesc "View a .score file"))
             ]

main :: IO ()
main =
  execParser (info (helper <*> cli) fullDesc) >>= execCli

execCli :: CLI -> IO ()
execCli (Render inp outp) =
  render (checkFormat (formatForFile outp)) inp outp
    where checkFormat (Just v) = v
          checkFormat Nothing = error $ "Unsupported file format for output: " <> outp
-- execCli (View xx) =
--   do tmpDir <- getTemporaryDirectory
--      tmpFile <- (tmpDir </>) . (++".pdf") . formatTime defaultTimeLocale "%q" <$> getCurrentTime
--      render PDF xx tmpFile
--      callCommand $ "open -n -W " <> tmpFile
--      x <- doesFileExist tmpFile
--      when x (removeFile tmpFile)
--      return ()

-- viewScore score =
--   do writeScorePage PDF "wizzle.pdf" score
--      callCommand "open -a Safari -g wizzle.pdf"
