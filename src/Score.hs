{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Score where

import           Control.Lens
import           Data.Sequence

someFunc :: IO ()
someFunc = putStrLn "whatever dudes"

-- | The core types

data Hand
  = L
  | R
  deriving (Eq,Show)

data Note =
  Note {_noteHand :: Hand
       ,_noteAccent :: Bool}
  deriving (Eq,Show)

newtype Beamed =
  Beamed (Seq Note)
  deriving (Eq,Show)

data Phrase =
  Phrase {_phraseAnacrusis :: Maybe Beamed
         ,_phraseBeams :: Seq Beamed}
  deriving (Eq,Show)

data Part =
  Part { _partPhrase :: Phrase,
         _partRepeat :: Repeat
       }
  deriving (Eq,Show)

data Repeat
  = NoRepeat
  | Repeat
  | Return Phrase
  deriving (Eq,Show)

newtype Score =
  Score (Seq Part)
  deriving (Eq,Show)

-- | Concrete lenses and prisms

makePrisms ''Hand
makeLenses ''Note
makeWrapped ''Beamed
makeLenses ''Phrase
makeLenses ''Part
makePrisms ''Repeat
makeWrapped ''Score

-- | Typeclass generalised members

class AsHand p f s where
  _Hand ::
    Optic' p f s Hand
  -- Optic is lens (set and get), iso (to and from), prism (0 or 1), traversal (0 - many)

class AsNote p f s where
  _Note ::
    Optic' p f s Note

class AsBeamed p f s where
  _Beamed ::
    Optic' p f s Beamed

instance AsHand p f Hand where
  _Hand = id

instance (p ~ (->),Functor f) => AsHand p f Note where
  _Hand = noteHand

instance AsNote p f Note where
  _Note = id

instance (Applicative f) => AsNote (->) f Beamed where
  _Note = _Wrapped . traverse

instance (Applicative f) => AsNote (->) f Phrase where
  _Note f (Phrase mb seqs) =
    Phrase <$> (_Just . _Note) f mb <*> (traverse . _Note) f seqs

instance (Applicative f) => AsNote (->) f Part where
  _Note f (Part ph rep) = Part <$> _Note f ph <*> pure rep

instance (Applicative f) => AsNote (->) f Score where
  _Note f (Score s) = Score <$> (traverse . _Note) f s

-- Utilities

swapH :: Hand -> Hand
swapH L = R
swapH R = L

_swapH :: Iso' Hand Hand
_swapH = iso swapH swapH

swapHands :: AsNote (->) Identity s
          => s -> s
swapHands = _Note . noteHand %~ swapH
