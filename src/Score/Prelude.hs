{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wwarn #-}
module Score.Prelude
       (
         module X,
         module Score.Prelude,
         (%)
       )

       where

import Score.Types as X
import Control.Lens as X
import Data.Semigroup as X
import Data.Ratio

(<->) = (<>)

infixl 0 <->

-- | General purpose utils

zap fs = partsOf _NoteHead %~ \vs -> zipWith (\v f -> f v) vs fs

zapN n f = elementOf _NoteHead n %~ f

-- | Note constructors
l1, l2, l4, l8, l16, l32, l64 :: Beamed
l1 = ln 1
l2 = ln (1/2)
l4 = ln (1/4)
l8 = ln (1/8)
l16 = ln (1/16)
l32 = ln (1/32)
l64 = ln (1/64)

r1, r2, r4, r8, r16, r32, r64 :: Beamed
r1 = rn 1
r2 = rn (1/2)
r4 = rn (1/4)
r8 = rn (1/8)
r16 = rn (1/16)
r32 = rn (1/32)
r64 = rn (1/64)

rest1, rest2, rest4, rest8, rest16, rest32, rest64 :: Beamed
rest1 = restn 1
rest2 = restn (1/2)
rest4 = restn (1/4)
rest8 = restn (1/8)
rest16 = restn (1/16)
rest32 = restn (1/32)
rest64 = restn (1/64)

ln, rn, restn :: Ratio Integer -> Beamed
ln = Beamed . pure . aNote L
rn = Beamed . pure . aNote R
restn = Beamed . pure . Rest

triplet :: Beamed -> Beamed
triplet = Beamed . pure . Tuplet (3 % 2)

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

dot = _Duration %~ (* (3/2))

cut = _Duration %~ (* (1/2))

-- | Bit builders

singles :: Int -> Beamed -> Beamed
singles n _ | n < 0 = error "bow bow"
singles 0 _ = Beamed mempty
singles n x = x <> singles (n-1) (x & swapHands)

-- start l = l .~ Just True

-- end l = l .~ Just False

-- clear l = l .~ Nothing
