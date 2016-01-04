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
                  , [rn (1/8), triplet [ln (1/16) & endRoll, rn (1/16), ln (1/16)]]
                  , [rn (1/8) & accentRoll, rn (1/8) & accent . endRoll]
                  , singles4Qtr
                    & _NoteHead . noteHeadDuration .~ (1/16)
                    & elementOf _NoteHead 0 %~ roll
                    & elementOf _NoteHead 1 %~ ((noteHeadHand .~ R) . endRoll )
                    & elementOf _NoteHead 2 . noteHeadEmbellishment .~ Just Drag
                  , singles4Qtr
                    & _NoteHead . noteHeadDuration .~ (1/16)
                    & elementOf _NoteHead 0 %~ ((noteHeadBuzz .~ False) . dot)
                    & elementOf _NoteHead 1 %~ cut
                    & elementOf _NoteHead 2 . noteHeadEmbellishment .~ Just Drag
                  ]

     writeFile "test.ly" (printScore music)
     callCommand "lilypond test.ly"
     callCommand "open -a Safari -g test.pdf"

accentRoll = accent . roll

roll = buzz . start (_NoteHead . noteHeadSlur)

endRoll = end (_NoteHead . noteHeadSlur)

-- L. R- L.f L.d

-- BITS

triplet = Tuplet (3 % 2)

dot = noteHeadDuration %~ (* (3/2))

cut = noteHeadDuration %~ (* (1/2))

singles4Qtr :: Beamed
singles4Qtr = [n R,n L,n R,n L]
         where n h = aNote h (1/4)

lFlam d = aNote L d & _NoteHead . noteHeadEmbellishment .~ Just Flam

rFlam d = aNote R d & _NoteHead . noteHeadEmbellishment .~ Just Flam

ln d = aNote L d
rn d = aNote R d

flam = _NoteHead . noteHeadEmbellishment .~ Just Flam

drag = _NoteHead . noteHeadEmbellishment .~ Just Drag

accent = _NoteHead . noteHeadAccent .~ True

buzz = _NoteHead . noteHeadBuzz .~ True

start l = l .~ Just True

end l = l .~ Just False

clear l = l .~ Nothing

aNote :: Hand -> Ratio Integer -> Note
aNote h d = Note $ NoteHead h False False d Nothing Nothing
