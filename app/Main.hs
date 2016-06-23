module Main where

import           Control.Concurrent.Chan      as Ch
import           Control.Exception            hiding (catch)
import           Control.Monad
import           Control.Monad.Catch
import           Language.Haskell.Interpreter
import           LilypondProcess
import           Options.Applicative
import           Score
import           Data.Time
import           Score.Types
import           System.Directory
import           System.FilePath
import           System.FSNotify
import           System.Process

data CLI
  = Watch Orientation FilePath String
  | Render Orientation [FilePath] String
  | View Orientation FilePath

cli :: Parser CLI
cli =
  (subparser . mconcat)
    [command "watch"
             (info (Watch <$> orientation <*>
                    strArgument (metavar "FILE") <*>
                    strArgument (metavar "VALUE"))
                   (progDesc "Watch a score for changes, preview when it does"))
    ,command "render"
             (info (Render <$>
                    orientation <*>
                    some (strOption (long "score-file")) <*>
                    strOption (long "output-file"))
              (progDesc "Render a .score file")
             )
    ,command "view" (info (View <$> orientation <*> strOption (long "score-file")) (progDesc "View a .score file"))
             ]
  where orientation =
          flag' Portrait (long "portrait") <|>
          flag' Landscape (long "landscape")

main :: IO ()
main =
  execParser (info (helper <*> cli) fullDesc) >>= execCli

execCli :: CLI -> IO ()
execCli (Watch o file valueName) =
  watch o file valueName
execCli (Render o inp outp) =
  render (checkFormat (formatForFile outp)) o inp outp
    where checkFormat (Just v) = v
          checkFormat Nothing = error $ "Unsupported file format for output: " <> outp
execCli (View o xx) =
  do tmpDir <- getTemporaryDirectory
     tmpFile <- (tmpDir </>) . (++".pdf") . formatTime defaultTimeLocale "%q" <$> getCurrentTime
     render PDF o [xx] tmpFile
     callCommand $ "open -n -W " <> tmpFile
     x <- doesFileExist tmpFile
     when x (removeFile tmpFile)


watch :: Orientation -> FilePath -> String -> IO ()
watch o file valueName = do
  absFile <- canonicalizePath file
  withManager $ \mgr ->
    do chan <- newChan
       -- Write an initial entry into the chan so we start displaying the results straight away
       writeChan chan (Modified absFile (error "Nick, u r going to regret this"))
       putStrLn ("Watching " <> file)
       watchChangesChan mgr chan file
       _ <- runInterpreterWoo (hintMe chan valueName o)
       return ()

runInterpreterWoo :: InterpreterT IO c -> IO c
runInterpreterWoo = runInterpreter >=> throwLeft

watchChangesChan :: WatchManager -> EventChannel -> FilePath -> IO ()
watchChangesChan mgr chan file =
  do absFile <- canonicalizePath file
     _ <- watchDirChan mgr (takeDirectory absFile) (changesTo absFile) chan
     return ()

changesTo :: FilePath -> Event -> Bool
changesTo f (Modified x _) = x == f
changesTo _ _ = False

throwLeft :: Either InterpreterError a -> IO a
throwLeft = either throwIO return

hintMe :: MonadInterpreter m => Chan Event -> String -> Orientation -> m b
hintMe chan valueName o =
  forever $
  do Modified f _ <- liftIO (readChan chan)
     let doIt = do
          score <- loadScores f valueName
          liftIO $ viewScore o score
     catch doIt (liftIO . printError)

loadScores :: MonadInterpreter m => FilePath -> String -> m [Score]
loadScores f valueName =
  do loadModules [f]
     setTopLevelModules =<< getLoadedModules
     interpret valueName (as :: [Score])

printError :: InterpreterError -> IO ()
printError = print

viewScore :: Orientation -> [Score] -> IO ()
viewScore orientation score =
  do writeScorePage orientation PDF "wizzle.pdf" score
     callCommand "open -a Safari -g wizzle.pdf"
