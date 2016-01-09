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

  -- where eg = tripletDotCut l8 & zapN 0 flam
  --       wat = tripletDotCut r8 & zap [flam, roll . swapHands, endRoll]
