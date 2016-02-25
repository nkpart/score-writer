{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
module Score.Types
       (
         module Score.Types
       )
       where

import           Control.Lens
import           Data.Ratio
import           Data.Semigroup
import Data.Foldable (foldl')
import           Data.Sequence   as S

type Toggle = Maybe Bool

toggle :: a -> a -> a -> Toggle -> a
toggle f g h = maybe f (\v -> bool v g h)
  where bool x a b = if x then b else a

-- | The core types

data Hand
  = L
  | R
  deriving (Eq,Show)

hand :: a -> a -> Hand -> a
hand a _ L = a
hand _ a R = a

data Embellishment
  = Flam
  | Drag
  | Ruff
  deriving (Eq,Show)

data NoteHead =
  NoteHead {_noteHeadHand :: Hand
           ,_noteHeadAccent :: Bool
           ,_noteHeadBuzz :: Bool
           ,_noteHeadDuration :: Ratio Integer
           ,_noteHeadSlurBegin :: Bool
           ,_noteHeadSlurEnd :: Bool
           ,_noteHeadEmbellishment :: Maybe Embellishment
           -- these should be applied to the note after this one
           ,_noteHeadMods :: [NoteMod]}
  deriving (Eq,Show)

data Note = Note NoteHead
          | Tuplet (Ratio Integer) (Seq Note)
          | Rest (Ratio Integer)
            deriving (Eq, Show)


data NoteMod = EndRoll deriving (Eq, Show)

-- TODO should be non empty
data Beamed =
  Beamed {_beamedNotes :: Seq Note }
  deriving (Eq,Show)

beam :: Note -> Beamed
beam = Beamed . pure

data Part =
  Part {_partAnacrusis :: Maybe Beamed
       ,_partBeams :: Seq Beamed
       ,_partRepeat :: Repeat}
  deriving (Eq,Show)

data Repeat
  = NoRepeat
  | Repeat
  | Return (Seq Beamed,Seq Beamed)
  deriving (Eq,Show)

data Signature = Signature Integer Integer deriving (Eq, Show)

data Score =
  Score {_scoreDetails :: Details
        ,_scoreSignature :: Signature
        ,_scoreParts :: Seq Part}
  deriving (Eq,Show)

data Details =
  Details {_detailsTitle :: String
          ,_detailsGenre :: String
          ,_detailsComposer :: String
          ,_detailsBand :: Maybe String}
  deriving (Eq,Show)

-- | Concrete lenses and prisms

makePrisms ''Hand
makeLenses ''NoteHead
makePrisms ''Note
makeLenses ''Beamed
-- makeLenses ''Phrase
makeLenses ''Part
makePrisms ''Repeat
makeLenses ''Details
makeLenses ''Score

-- | Typeclass generalised members

type Duration = Ratio Integer

class AsHand p f s where
  _Hand ::
    Optic' p f s Hand

class AsDuration p f s where
  _Duration ::
    Optic' p f s (Ratio Integer)

class AsNoteHead p f s where
  _NoteHead ::
    Optic' p f s NoteHead

class AsBeamed p f s where
  _Beamed ::
    Optic' p f s Beamed

instance AsBeamed p f Beamed where _Beamed = id
instance (Applicative f, p ~ (->)) => AsBeamed p f (Seq Beamed) where _Beamed = traverse . _Beamed

instance (Applicative f, p ~ (->)) => AsBeamed p f ([Beamed]) where _Beamed = traverse . _Beamed


class AsSignature p f s where
  _Signature :: Optic' p f s Signature

instance AsHand p f Hand where
  _Hand = id

instance (p ~ (->),Functor f) => AsHand p f NoteHead where
  _Hand = noteHeadHand

instance (p ~ (->),Applicative f) => AsHand p f Note where
  _Hand f (Note n) = Note <$> _Hand f n
  _Hand _ (Rest n) = pure (Rest n)
  _Hand f (Tuplet r n) = Tuplet r <$> (traverse . _NoteHead . _Hand) f n

instance (p ~ (->),Applicative f) => AsHand p f Beamed where
  _Hand f (Beamed n) = Beamed <$> (traverse . _Hand) f n -- <*> pure m

instance AsDuration p f Duration where
  _Duration = id

instance (p ~ (->),Functor f) => AsDuration p f NoteHead where
  _Duration = noteHeadDuration

instance (p ~ (->),Applicative f) => AsDuration p f Note where
  _Duration f (Note n) = Note <$> _Duration f n
  _Duration f (Rest n) = Rest <$> f n
  _Duration f (Tuplet r n) = Tuplet r <$> (traverse . _Duration) f n

instance (p ~ (->),Applicative f) => AsDuration p f Beamed where
  _Duration f (Beamed n) = Beamed <$> (traverse . _Duration) f n -- <*> pure m

instance AsNoteHead p f NoteHead where
  _NoteHead = id

instance Applicative f => AsNoteHead (->) f Note where
  _NoteHead f (Note h) = Note <$> f h
  _NoteHead _ (Rest n) = pure (Rest n)
  _NoteHead f (Tuplet d ns) = Tuplet d <$> (traverse . _NoteHead) f ns

instance (Applicative f) => AsNoteHead (->) f Beamed where
  _NoteHead = beamedNotes . traverse . _NoteHead

instance (Applicative f) => AsNoteHead (->) f (Seq Beamed) where
  _NoteHead = traverse . _NoteHead

instance (Applicative f) => AsNoteHead (->) f [Beamed] where
  _NoteHead = traverse . _NoteHead

instance (Applicative f) => AsNoteHead (->) f Part where
  -- TODO should this touch the anacrusis?
  _NoteHead f (Part ana ph rep) = Part <$> (traverse . _NoteHead) f ana <*> (traverse . _NoteHead) f ph <*> pure rep

instance (Applicative f) => AsNoteHead (->) f Score where
  _NoteHead f (Score d sig s) = Score d sig <$> (traverse . _NoteHead) f s

instance AsSignature p f Signature where
  _Signature = id

instance (p ~ (->), Functor f) => AsSignature p f Score where
  _Signature = scoreSignature

-- | ???

swapH :: Hand -> Hand
swapH L = R
swapH R = L

_swapH :: Iso' Hand Hand
_swapH = iso swapH swapH

swapHands :: AsHand (->) Identity s
          => s -> s
swapHands = _Hand %~ swapH

applyMods :: [NoteMod] -> NoteHead -> NoteHead
applyMods xs a =
  foldl' (\h EndRoll -> h & noteHeadSlurEnd .~ True) a xs

instance Semigroup Beamed where
  Beamed a <> Beamed b = Beamed (a <> b)
