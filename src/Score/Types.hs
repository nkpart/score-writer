{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
module Score.Types where

import           Control.Lens
import           Data.Ratio
import           Data.Sequence as S
import           GHC.Exts      (IsList, Item, fromList, toList)

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
  deriving (Eq,Show)

data NoteHead =
  NoteHead {_noteHeadHand :: Hand
       ,_noteHeadAccent :: Bool
       ,_noteHeadBuzz :: Bool
       ,_noteHeadDuration :: Ratio Integer
       ,_noteHeadSlur :: Maybe Bool
       ,_noteHeadEmbellishment :: Maybe Embellishment}
  deriving (Eq,Show)


data Note = Note NoteHead
          | Tuplet (Ratio Integer) (Seq Note)
            deriving (Eq, Show)

newtype Beamed =
  Beamed (Seq Note)
  deriving (Eq,Show)

instance IsList Beamed where
  type Item Beamed = Note
  fromList = Beamed . S.fromList
  toList (Beamed b) = toList b

data Part =
  Part {_partRepeat :: Repeat
       ,_partAnacrusis :: Maybe Beamed
       ,_partBeams :: Seq Beamed}
  deriving (Eq,Show)

data Repeat
  = NoRepeat
  | Repeat
  | Return (Seq Beamed)
  deriving (Eq,Show)

data Score =
  Score
  { _scoreSignature :: (Integer, Integer), _scoreParts :: Seq Part}
  deriving (Eq,Show)

-- | Concrete lenses and prisms

makePrisms ''Hand
makeLenses ''NoteHead
makePrisms ''Note
makeWrapped ''Beamed
-- makeLenses ''Phrase
makeLenses ''Part
makePrisms ''Repeat
--- makeXXX ''Score

-- | Typeclass generalised members

class AsHand p f s where
  _Hand ::
    Optic' p f s Hand
  -- Optic is lens (set and get), iso (to and from), prism (0 or 1), or traversal (0 - many), or fold (get many)

class AsDuration p f s where
  _Duration ::
    Optic' p f s (Ratio Integer)

class AsNoteHead p f s where
  _NoteHead ::
    Optic' p f s NoteHead

class AsBeamed p f s where
  _Beamed ::
    Optic' p f s Beamed

instance AsHand p f Hand where
  _Hand = id

instance (p ~ (->),Functor f) => AsHand p f NoteHead where
  _Hand = noteHeadHand

instance AsNoteHead p f NoteHead where
  _NoteHead = id

instance Applicative f => AsNoteHead (->) f Note where
  _NoteHead f (Note h) = Note <$> f h
  _NoteHead f (Tuplet d ns) = Tuplet d <$> (traverse . _NoteHead) f ns

instance (Applicative f) => AsNoteHead (->) f Beamed where
  _NoteHead = _Wrapped . traverse . _NoteHead

instance (Applicative f) => AsNoteHead (->) f Part where
  _NoteHead f (Part rep x ph) = Part rep x <$> (traverse . _NoteHead) f ph

instance (Applicative f) => AsNoteHead (->) f Score where
  _NoteHead f (Score sig s) = Score sig <$> (traverse . _NoteHead) f s

-- | ???

swapH :: Hand -> Hand
swapH L = R
swapH R = L

_swapH :: Iso' Hand Hand
_swapH = iso swapH swapH

swapHands :: AsHand (->) Identity s
          => s -> s
swapHands = _Hand %~ swapH
