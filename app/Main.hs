module Main where

import Score
import Score.Types (Score)
-- import Score.Library.BBCOCA
import System.Process
import Control.Monad.Catch
import System.FilePath
import System.FSNotify
import System.Directory
import Data.Monoid ((<>))
import Control.Concurrent.Chan as Ch
import Control.Monad (forever)
import Control.Exception hiding (catch)
import Language.Haskell.Interpreter
import System.Environment

main :: IO ()
main = do
  file:value:_ <- getArgs
  -- let file = "src/Score/Library/BBCOCA.hs"
  --     value = "pmDonaldMacleanOfLewis"

  absFile <- canonicalizePath file

  withManager $ \mgr ->
    do chan <- newChan
  -- Write an initial entry into the chan so we start displaying the results straight away
       writeChan chan (Modified absFile (error "Nick, u r going to regret this"))
       putStrLn ("Watching " <> file)
       watchChangesChan mgr chan file
       throwLeft_ =<< runInterpreter (hintMe chan value)

watchChangesChan mgr chan file =
  do absFile <- canonicalizePath file
     _ <- watchDirChan mgr (takeDirectory absFile) (changesTo absFile) chan
     return ()

changesTo f (Modified x _) = x == f
changesTo _ _ = False

throwLeft = either throwIO return

throwLeft_ = fmap (const ()) . throwLeft

hintMe chan valueName =
  forever $
  do Modified f _ <- liftIO (readChan chan)
     let action = do
          loadModules [f]
          setTopLevelModules =<< getLoadedModules
          score <- interpret valueName (as :: Score)
          liftIO $ viewScore score
     catch action (liftIO . printError)

printError :: InterpreterError -> IO ()
printError = print

viewScore score =
  do writeScorePDF "wizzle" score
     callCommand $ "open -a Safari -g wizzle.pdf"
