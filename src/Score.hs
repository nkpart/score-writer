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
import           Data.Sequence (Seq)


a24 :: Score

a24 = Score (2,4) (Just [l8]) [p1, p2, p3, p4]
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
        

aTune :: IO ()
aTune =
  do let music =
           Score (6,8) (Just [l8]) [p1, p2, p3, p4]
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

     writeFile "test.ly" (printScore music)
     callCommand "lilypond test.ly"
     callCommand "open -a Safari -g test.pdf"

tripletDotCut x = triplet [x & dot, x & cut . swapHands, x]

  -- where eg = tripletDotCut l8 & zapN 0 flam
  --       wat = tripletDotCut r8 & zap [flam, roll . swapHands, endRoll]

-- | General purpose utils

zap fs = partsOf _NoteHead %~ zipWith ($) fs

zapN n f = elementOf _NoteHead n %~ f

-- | Note constructors

l1, l2, l4, l8, l16, l32, l64 :: Note
l1 = ln 1
l2 = ln (1/2)
l4 = ln (1/4)
l8 = ln (1/8)
l16 = ln (1/16)
l32 = ln (1/32)
l64 = ln (1/64)

r1, r2, r4, r8, r16, r32, r64 :: Note
r1 = rn 1
r2 = rn (1/2)
r4 = rn (1/4)
r8 = rn (1/8)
r16 = rn (1/16)
r32 = rn (1/32)
r64 = rn (1/64)

ln, rn :: Ratio Integer -> Note
ln = aNote L
rn = aNote R

triplet :: Seq Note -> Note
triplet = Tuplet (3 % 2)

aNote :: Hand -> Ratio Integer -> Note
aNote h d = Note $ NoteHead h False False d False False Nothing

-- | Note modifiers

buzz = _NoteHead . noteHeadBuzz .~ True

roll = buzz . (_NoteHead . noteHeadSlurBegin .~ True)

endRoll = _NoteHead . noteHeadSlurEnd .~ True

flam = _NoteHead . noteHeadEmbellishment .~ Just Flam

drag = _NoteHead . noteHeadEmbellishment .~ Just Drag

ruff = _NoteHead . noteHeadEmbellishment .~ Just Ruff

accent = _NoteHead . noteHeadAccent .~ True

dot = _NoteHead . noteHeadDuration %~ (* (3/2))

cut = _NoteHead . noteHeadDuration %~ (* (1/2))


-- | Bit builders

singles :: Int -> Note -> [Note]
singles n _ | n < 0 = error "bow bow"
singles 0 _ = []
singles n x = x : singles (n-1) (x & swapHands)

-- start l = l .~ Just True

-- end l = l .~ Just False

-- clear l = l .~ Nothing

