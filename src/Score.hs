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
import           LilypondProcess
import           Score.Prelude
import           Score.Render
import           Score.Types

writeScorePNG name music = runLilypond PNG name (printScore music)

writeScorePDF name music = runLilypond PDF name (printScore music)

a24 :: Score
a24 = Score
  (Details "93rd at Modder River"
           "March"
           "Raymond Malcolm"
           Nothing)
  (2,4)
  (Just [l8]) [p1, p2, p3, p4]
  where p1 = Part [[r16 & flam . accent, l32, l32, r16 & dot, l16 & flam . cut] ,[r32, l32,l32,r32,l16 & dot,l16 & cut . accent]
                  ,fromList (triplet [r16, l16, r16 & drag] : singles 4 r32) ,[r8 & roll, triplet [l16,r16,l16]]
                  ,[r16 & flam . accent . dot, l16 & cut, r16 & flam . dot, l16 & cut . flam], fromList $ singles 4 r32 <> [r16, triplet [r16 & flam, l16, r16 & accent]]
                  ,[l16 & dot, l16 & cut, r16 & flam, triplet [l32, r32, r32] ], [l16 & dot, l16 & cut . flam, r32,l32,r32,l32]
                  ] Repeat
        p2 = Part [] Repeat
        p3 = Part [] Repeat
        p4 = Part [] Repeat

        returnPart =
          [[r16 & flam, l32, l32] <> singles 4 r32, [r16 & dot, l16 & cut, r16 & flam . dot, l16 & flam . cut]
          ,triplet [r16, l16, r16 & drag] : singles 4 r32, [r16 & accent, triplet [l32, r32, r32], l16 & dot, l16 & cut . flam] ]

        ending =
          [[r32, l32, l32, r32, l16 & dot . roll, l16 & cut], triplet [r16, l16, r16 & drag] : singles 4 r32
          ,[r8 & accent, r8 & roll], [l4 & endRoll]]

pmDonaldMacleanOfLewis =
  Score (Details "Pipe Major Donald Maclean of Lewis"
                 "March"
                 "Nick Partridge"
                 (Just "BBC Old Collegians Pipe Band"))
        (6,8)
        (Just [l8]) [p1, p2, p3, p4]
     where
         p1 =
             Part (firstBeginning <> firstEnding <> firstBeginning <> secondEnding) Repeat
         firstBeginning =
                  [[r8 & flam . dot, r8 & roll . cut, r8 & endRoll] , [l4 & flam], [r8 & roll]
                  ,[triplet [l8 & accent . endRoll, r8, l8], r8 & flam] , [l8 & flam . dot, r8 & cut, l8]]
         p2 =
           let beginning =
                 [[r4 & roll. dot], [r4 & endRoll], [r8 & roll],
                  [l8 & endRoll . accent, r8 & roll, r8 & endRoll],
                  [l8 & flam . dot, r8 & cut, l8]]
           in Part
                   (beginning <> firstEnding)
                   (Return (beginning <> secondEnding, pReturn))
         p3 =
           let beginning =
                 [[r8 & flam. dot, r8 & roll . cut, r8 & endRoll], [triplet [l8, r8, l8], r8 & flam]
                 ,[triplet [l8, r8, l8], r8 & flam], [l8 & flam . dot, r8 & cut, l8]]
            in Part (beginning <> firstEnding <> beginning <> secondEnding) Repeat
         p4 =
           let beginning =
                 [[r4 & flam], [l8 & ruff], [r4 & flam], [r8 & roll]
                 ,[triplet [l8 & endRoll . accent, r8, l8], r8 & flam], [l8 & flam . dot, r8 & cut, l8]]
            in Part (beginning <> firstEnding) (Return (beginning <> secondEnding, pReturn))
         pReturn = firstBeginning <> secondEnding
         firstEnding =
           [[r8 & roll, triplet [l8 & endRoll, r8, l8]]
           ,[Tuplet (2 % 3) [r8 & roll . accent, l8 & roll . endRoll . accent]]
           ,[r16 & endRoll, l16,r16,l16,r8]
           ,[l8 & dot, r8 & cut, l8]]
         secondEnding =
           [[r8 & flam . dot, r8 & roll . cut, r8 & endRoll ],
            [r8 & roll . dot, r8 & endRoll . cut, l8 & accent . roll],
            [r8 & endRoll . dot, l8 & cut, r8],
            [l4 & flam], [ l8 ]]

tripletDotCut x = triplet [x & dot, x & cut . swapHands, x]

  -- where eg = tripletDotCut l8 & zapN 0 flam
  --       wat = tripletDotCut r8 & zap [flam, roll . swapHands, endRoll]
