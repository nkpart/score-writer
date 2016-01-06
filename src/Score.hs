{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE TypeFamilies              #-}
module Score where

import           Control.Lens
import           Data.Monoid
import           Data.Ratio
import           Score.Render
import           Score.Types
import           System.Process

aTune :: IO ()
aTune =
  do let music =
           Score (6,8) [firstPart, secondPart]
         firstPart =
             Part Nothing (firstBeginning <> firstEnding <> firstBeginning <> secondEnding) Repeat
         firstBeginning =
                  [[r8 & flam . dot, r8 & roll . cut, r8 & endRoll] , [l4 & flam], [r8 & roll]
                  , [triplet [l8 & accent . endRoll, r8, l8], r8 & flam] , [l8 & flam . dot, r8 & cut, l8]]
         secondPart =
           let beginning =
                 [[r4 & roll. dot], [r4 & endRoll], [r8 & roll],
                  [l8 & endRoll . accent, r8 & roll, r8 & endRoll],
                  [l8 & flam . dot, r8 & cut, l8]]
           in Part Nothing
                   (beginning <> firstEnding)
                   (Return (beginning <> secondEnding, firstBeginning <> secondEnding))
         firstEnding =
           [[r8 & roll, triplet [l8 & endRoll, r8, l8]]
           , [Tuplet (2 % 3) [r8 & roll . accent, l8 & roll . endRoll . accent]]
           , [r16 & endRoll, l16,r16,l16,r8]
           , [l8 & dot, r8 & cut, l8]]
         secondEnding =
           [[r8 & flam . dot, r8 & roll . cut, r8 & endRoll ],
             [r8 & roll . dot, r8 & endRoll . cut, l8 & accent . roll],
             [r8 & endRoll . dot, l8 & cut, r8],
             [l4 & flam . dot]]

     writeFile "test.ly" (printScore music)
     callCommand "lilypond test.ly"
     callCommand "open -a Safari -g test.pdf"

accentRoll = accent . roll

roll = buzz . (_NoteHead . noteHeadSlurBegin .~ True)

endRoll = _NoteHead . noteHeadSlurEnd .~ True

triplet = Tuplet (3 % 2)

dot = _NoteHead . noteHeadDuration %~ (* (3/2))

cut = _NoteHead . noteHeadDuration %~ (* (1/2))

singles4Qtr :: Beamed
singles4Qtr = [n R,n L,n R,n L]
         where n h = aNote h (1/4)

lFlam d = aNote L d & _NoteHead . noteHeadEmbellishment .~ Just Flam

rFlam d = aNote R d & _NoteHead . noteHeadEmbellishment .~ Just Flam

ln d = aNote L d
rn d = aNote R d

l4 = ln (1/4)
l8 = ln (1/8)
l16 = ln (1/16)
l32 = ln (1/32)

r4 = rn (1/4)
r8 = rn (1/8)
r16 = rn (1/16)
r32 = rn (1/32)

flam = _NoteHead . noteHeadEmbellishment .~ Just Flam

drag = _NoteHead . noteHeadEmbellishment .~ Just Drag

accent = _NoteHead . noteHeadAccent .~ True

buzz = _NoteHead . noteHeadBuzz .~ True

start l = l .~ Just True

end l = l .~ Just False

clear l = l .~ Nothing

aNote :: Hand -> Ratio Integer -> Note
aNote h d = Note $ NoteHead h False False d False False Nothing
