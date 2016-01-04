{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE TypeFamilies              #-}
module Score where

import           Control.Lens
import           Data.Ratio
import           Score.Render
import           Score.Types
import           System.Process

someFunc :: IO ()
someFunc =
  do let music = Score (4,4) . pure . Part NoRepeat Nothing $
                 [  [rFlam (1/4)]
                  , [lFlam (1/8), rn (1/8) & roll]
                  , [rn (1/8) & endRoll, ln (1/8)]
                  , [rn (1/8) & accentRoll, rn (1/8) & accent . endRoll]
                  , singles4Qtr
                    & _Note . noteDuration .~ (1/16)
                    & elementOf _Note 0 %~ roll
                    & elementOf _Note 1 %~ ((noteHand .~ R) . endRoll )
                    & elementOf _Note 2 . noteEmbellishment .~ Just Drag
                  , singles4Qtr
                    & _Note . noteDuration .~ (1/16)
                    & elementOf _Note 0 %~ ((noteBuzz .~ False) . dot)
                    & elementOf _Note 1 %~ cut
                    & elementOf _Note 2 . noteEmbellishment .~ Just Drag
                  ]

     writeFile "test.ly" (printScore music)
     callCommand "lilypond test.ly"
     callCommand "open -a Safari -g test.pdf"

accentRoll = accent . roll

roll = buzz . start noteSlur

endRoll = end noteSlur

-- L. R- L.f L.d

-- BITS

dot :: Note -> Note
dot = noteDuration %~ (* (3/2))

cut :: Note -> Note
cut = noteDuration %~ (* (1/2))

singles4Qtr :: Beamed
singles4Qtr = [n R,n L,n R,n L]
         where n h = aNote h (1/4)

lFlam d = aNote L d & noteEmbellishment .~ Just Flam

rFlam d = aNote R d & noteEmbellishment .~ Just Flam

ln d = aNote L d
rn d = aNote R d

flam = noteEmbellishment .~ Just Flam

drag = noteEmbellishment .~ Just Drag

accent = noteAccent .~ True

buzz = noteBuzz .~ True

start l = l .~ Just True

end l = l .~ Just False

clear l = l .~ Nothing

aNote :: Hand -> Ratio Integer -> Note
aNote h d = Note h False False d Nothing Nothing
