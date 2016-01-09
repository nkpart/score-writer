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
       , GHC.Exts.fromList
       )
       where

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
  | Ruff
  deriving (Eq,Show)

data NoteHead =
  NoteHead {_noteHeadHand :: Hand
           ,_noteHeadAccent :: Bool
           ,_noteHeadBuzz :: Bool
           ,_noteHeadDuration :: Ratio Integer
           ,_noteHeadSlurBegin :: Bool
           ,_noteHeadSlurEnd :: Bool
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
  Part {
        _partBeams :: Seq Beamed
       ,_partRepeat :: Repeat}
  deriving (Eq,Show)

data Repeat
  = NoRepeat
  | Repeat
  | Return (Seq Beamed, Seq Beamed)
  deriving (Eq,Show)

data Score =
  Score
  { _scoreDetails :: Details,
    _scoreSignature :: (Integer, Integer),
    _scoreAnacrusis :: Maybe Beamed,
    _scoreParts :: Seq Part}
  deriving (Eq,Show)

data Details =
  Details
  { _detailsTitle :: String,
    _detailsGenre :: String,
    _detailsComposer :: String,
    _detailsBand :: Maybe String
  } deriving (Eq, Show)

-- | Concrete lenses and prisms

makePrisms ''Hand
makeLenses ''NoteHead
makePrisms ''Note
makeWrapped ''Beamed
-- makeLenses ''Phrase
makeLenses ''Part
makePrisms ''Repeat
makeLenses ''Details
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

instance (p ~ (->),Applicative f) => AsHand p f Note where
  _Hand f (Note n) = Note <$> _Hand f n
  _Hand f (Tuplet r n) = Tuplet r <$> (traverse . _Hand) f n

instance AsNoteHead p f NoteHead where
  _NoteHead = id

instance Applicative f => AsNoteHead (->) f Note where
  _NoteHead f (Note h) = Note <$> f h
  _NoteHead f (Tuplet d ns) = Tuplet d <$> (traverse . _NoteHead) f ns

instance (Applicative f) => AsNoteHead (->) f Beamed where
  _NoteHead = _Wrapped . traverse . _NoteHead

instance (Applicative f) => AsNoteHead (->) f Part where
  _NoteHead f (Part ph rep) = Part <$> (traverse . _NoteHead) f ph <*> pure rep

instance (Applicative f) => AsNoteHead (->) f Score where
  -- TODO should this touch the anacrusis?
  _NoteHead f (Score d sig ana s) = Score d sig ana <$> (traverse . _NoteHead) f s

-- | ???

swapH :: Hand -> Hand
swapH L = R
swapH R = L

_swapH :: Iso' Hand Hand
_swapH = iso swapH swapH

swapHands :: AsHand (->) Identity s
          => s -> s
swapHands = _Hand %~ swapH
