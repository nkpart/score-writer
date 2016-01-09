{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Score.Prelude where

import Score.Types
import Data.Ratio
import Data.Sequence (Seq)
import Control.Lens

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
