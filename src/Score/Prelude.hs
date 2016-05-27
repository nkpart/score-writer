{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
module Score.Prelude
       (
         module X,
         module Score.Prelude,
         (%)
       )

       where

import Score.Types as X
import Control.Lens as X hiding (para)
import Data.Semigroup as X
import Data.Ratio
import Control.Monad.Writer hiding ((<>))
import Control.Lens.Internal.Bazaar
import Control.Lens.Internal.Indexed

(<->) :: Semigroup a => a -> a -> a
(<->) = (<>)

infixl 0 <->

-- | General purpose utils

zap :: AsNoteHead (->) (BazaarT (->) Identity NoteHead NoteHead) t
    => [NoteHead -> NoteHead] -> t -> t
zap fs = partsOf _NoteHead %~ \vs -> zipWith (\v f -> f v) vs fs

zapN :: AsNoteHead (->) (Indexing Identity) t
     => Int -> (NoteHead -> NoteHead) -> t -> t
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
ln = beam . aNote L
rn = beam . aNote R
restn = beam . aRest

aRest :: Ratio Integer -> Note
aRest = Rest

triplet :: Beamed -> Beamed
triplet = tuplet 3 2

tuplet :: Integer -> Integer -> Beamed -> Beamed
tuplet a b (Beamed n) = Beamed (pure $ Tuplet (a % b) n)

startUnison :: Beamed
startUnison = Beamed (pure $ U StartUnison)

stopUnison :: Beamed
stopUnison = Beamed (pure $ U StopUnison)

thisUnison :: Beamed -> Beamed
thisUnison x = startUnison <-> x <-> stopUnison

aNote :: Hand -> Ratio Integer -> Note
aNote h d = Note $ NoteHead h NoAccent False d False False Nothing False Nothing [] Nothing

-- | Note modifiers

buzz :: AsNoteHead (->) Identity t => t -> t
buzz = _NoteHead . noteHeadBuzz .~ True

startRoll :: AsNoteHead (->) Identity c => c -> c
startRoll = buzz . (_NoteHead . noteHeadSlurBegin .~ True)

endRoll :: AsNoteHead (->) Identity t => t -> t
endRoll = _NoteHead . noteHeadSlurEnd .~ True

dynamics :: AsNoteHead (->) Identity t => Dynamics -> t -> t
dynamics p = _NoteHead . noteHeadDynamics .~ Just p

roll :: AsNoteHead (->) Identity c => c -> c
roll = (_NoteHead . noteHeadMods %~ (EndRoll:)) . startRoll

flam :: AsNoteHead (->) Identity t => t -> t
flam = _NoteHead . noteHeadEmbellishment .~ Just Flam

drag :: AsNoteHead (->) Identity t => t -> t
drag = _NoteHead . noteHeadEmbellishment .~ Just Drag

ruff :: AsNoteHead (->) Identity t => t -> t
ruff = _NoteHead . noteHeadEmbellishment .~ Just Ruff

accent :: AsNoteHead (->) Identity t => t -> t
accent = _NoteHead . noteHeadAccent .~ AccentRegular

bigAccent :: AsNoteHead (->) Identity t => t -> t
bigAccent = _NoteHead . noteHeadAccent .~ AccentBig

startCrescendo :: AsNoteHead (->) Identity t => t -> t
startCrescendo =
  _NoteHead . noteHeadCrescBegin .~ Just Cresc

startDecrescendo :: AsNoteHead (->) Identity t => t -> t
startDecrescendo =
  _NoteHead . noteHeadCrescBegin .~ Just Decresc

endCresc :: AsNoteHead (->) Identity t => t -> t
endCresc =
  _NoteHead . noteHeadCrescEnd .~ True

dot :: AsDuration (->) Identity t => t -> t
dot = _Duration %~ (* (3/2))

cut :: AsDuration (->) Identity t => t -> t
cut = _Duration %~ (* (1/2))

-- | Bit builders

singles :: Int -> Beamed -> Beamed
singles n _ | n <= 0 = error "bow bow"
singles 1 x = x
singles n x = x <> singles (n-1) (x & swapHands)

h2h :: Beamed -> [NoteHead -> NoteHead] -> Beamed
h2h startNote fs = singles (Prelude.length fs) startNote & zap fs

para :: (Semigroup a, AsHand (->) Identity a) => a -> a
para x = x <-> swapHands x <-> x <-> x

dotCut :: AsNoteHead (->) (BazaarT (->) Identity NoteHead NoteHead) t
       => t -> t
dotCut = zap (cycle [dot, cut])

-- | DSL

type PartM a = Writer (Endo Part) a

writeF :: (Part -> Part) -> PartM ()
writeF f = tell $ Endo f

buildPart :: PartM a -> Part
buildPart ma = appEndo (execWriter ma) (Part Nothing mempty NoRepeat)

upbeat :: Beamed -> PartM ()
upbeat v = writeF (partAnacrusis .~ Just v)

bars :: [Beamed] -> PartM ()
bars bs = writeF (partBeams %~ (\x -> bs <> x))

firstTime :: [Beamed] -> PartM ()
firstTime x = writeF (partRepeat %~ f)
  where f NoRepeat = Return x mempty
        f Repeat = Return x mempty
        f (Return p q) = Return (p <> x) q

secondTime :: [Beamed] -> PartM ()
secondTime x = writeF (partRepeat %~ f)
  where f NoRepeat = Return mempty x
        f Repeat = Return mempty x
        f (Return p q) = Return p (q <> x)

thenRepeat :: PartM ()
thenRepeat = writeF (partRepeat .~ Repeat)

noRepeat :: PartM ()
noRepeat = writeF (partRepeat .~ NoRepeat)
