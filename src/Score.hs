{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE TypeFamilies              #-}
module Score where

import           LilypondProcess
import           Score.Render
import           Score.Types

writeScorePNG :: FilePath -> Score -> IO ()
writeScorePNG name music = runLilypond PNG name (printScore music)

writeScorePDF :: FilePath -> Score -> IO ()
writeScorePDF name music = runLilypond PDF name (printScore music)
