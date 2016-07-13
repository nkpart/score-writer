{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
module Score.Types
       (
         module Score.Types
       )
       where

import           Control.Lens
import           Data.Data
import           Data.Foldable  (foldl')
import           Data.Ratio
import           Data.Semigroup
import           Data.List.NonEmpty

-- | The core types

data Hand
  = L
  | R
  deriving (Eq, Show, Data, Typeable)

hand :: a -> a -> Hand -> a
hand a _ L = a
hand _ a R = a

data Embellishment
  = Flam
  | Drag
  | Ruff
  | Ratamacue
  deriving (Eq, Show, Data, Typeable)

data Unison =
  StartUnison | StopUnison deriving (Eq, Show, Data, Typeable)

data NoteHead =
  NoteHead {_noteHeadHand :: Hand
           ,_noteHeadAccent :: AccentSize
           ,_noteHeadBuzz :: Bool
           ,_noteHeadDuration :: Ratio Integer
           ,_noteHeadSlurBegin :: Bool
           ,_noteHeadSlurEnd :: Bool
           ,_noteHeadCrescBegin :: Maybe Cresc
           ,_noteHeadCrescEnd :: Bool
           ,_noteHeadEmbellishment :: Maybe Embellishment
           -- these should be applied to the note after this one
           ,_noteHeadMods :: [NoteMod]
           ,_noteHeadDynamics :: Maybe Dynamics
           }
  deriving (Eq, Show, Data, Typeable)

data Cresc = Cresc | Decresc deriving (Eq, Show, Data, Typeable)
data AccentSize = NoAccent |  AccentRegular | AccentBig deriving (Eq, Show, Data, Typeable)

data Note = Note NoteHead
          | Tuplet (Ratio Integer) (NonEmpty Note)
          | Rest (Ratio Integer)
          | U Unison
            deriving (Eq, Show, Data, Typeable)

data NoteMod = EndRoll deriving (Eq, Show, Data, Typeable)

data Dynamics
  = PPPPP | PPPP | PPP | PP | P
  | MP | MF
  | F | FF | FFF | FFFF
  | SF | SFF
  | SP | SPP
  | SFZ | RFZ
  deriving (Eq,Show,Data,Typeable,Enum)

newtype Beamed =
  Beamed { _beamedNotes :: NonEmpty Note }
  deriving (Eq, Show, Data, Typeable)


data Bar =
    Bar (Maybe Signature) [Beamed]
  | PartialBar Beamed
  -- TODO | RepeatBar
  deriving (Eq, Show, Data, Typeable)

data Part =
  Part {_partBars :: [Bar]
       ,_partRepeat :: Repeat}
  deriving (Eq,Show)

data Repeat
  = NoRepeat
  | Repeat
  | Return [Bar] [Bar]
  deriving (Eq,Show)

data Signature = Signature Integer Integer deriving (Eq, Show, Data)

signatureDuration :: Signature -> Ratio Integer
signatureDuration (Signature n d) = n % d

data Score =
  Score {_scoreDetails :: Details
        ,_scoreSignature :: Signature
        ,_scoreParts :: [Part]}
  deriving (Eq,Show)

data Details =
  Details {_detailsTitle :: String
          ,_detailsGenre :: String
          ,_detailsComposer :: String
          ,_detailsBand :: Maybe String}
  deriving (Eq,Show)

blankDetails :: Details
blankDetails =
  Details "" "" "" Nothing

-- | Concrete lenses and prisms

makePrisms ''Hand
makeLenses ''NoteHead
makePrisms ''Note
makeLenses ''Beamed
makeLenses ''Part
makePrisms ''Repeat
makePrisms ''Bar
makeLenses ''Details
makeLenses ''Score

-- | Typeclass generalised members

type Duration = Ratio Integer

class AsHand p f s where
  _Hand ::
    Optic' p f s Hand

class AsDuration p f s where
  _Duration ::
    Optic' p f s Duration

instance AsDuration p f Duration where
  _Duration = id

class AsNoteHead p f s where _NoteHead :: Optic' p f s NoteHead

class AsBeamed p f s where
  _Beamed ::
    Optic' p f s Beamed

instance AsBeamed p f Beamed where _Beamed = id

instance (Applicative f, p ~ (->)) => AsBeamed p f [Beamed] where _Beamed = traverse . _Beamed

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
  _Hand _ (U u) = pure (U u)
  -- _Hand _ StopUnison = pure StopUnison

instance (p ~ (->),Applicative f) => AsHand p f Beamed where
  _Hand f (Beamed n) = Beamed <$> (traverse . _Hand) f n -- <*> pure m

instance (p ~ (->),Functor f) => AsDuration p f NoteHead where
  _Duration = noteHeadDuration

instance (p ~ (->),Applicative f) => AsDuration p f Note where
  _Duration f (Note n) = Note <$> _Duration f n
  _Duration f (Rest n) = Rest <$> f n
  _Duration f (Tuplet r n) = Tuplet r <$> (traverse . _Duration . tupletise r) f n
  _Duration _ (U u) = pure (U u)

-- HOLY TEST ME BATMAN
tupletise :: Ratio Integer -> Simple Iso (Ratio Integer) (Ratio Integer)
tupletise x =
  let r = numerator x
      n = denominator x
   in iso (n%r *) (r%n *)

instance (p ~ (->),Applicative f) => AsDuration p f Beamed where
  _Duration f (Beamed n) = Beamed <$> (traverse . _Duration) f n

instance (p ~ (->),Applicative f) => AsDuration p f [Beamed] where
  _Duration = traverse . _Duration

instance (Applicative f) => AsDuration (->) f Bar where
  _Duration f (Bar a bs) = Bar a <$> _Duration f bs
  _Duration f (PartialBar bs) = PartialBar <$> _Duration f bs

instance AsNoteHead p f NoteHead where
  _NoteHead = id

instance Applicative f => AsNoteHead (->) f Note where
  _NoteHead f (Note h) = Note <$> f h
  _NoteHead _ (Rest n) = pure (Rest n)
  _NoteHead f (Tuplet d ns) = Tuplet d <$> (traverse . _NoteHead) f ns
  _NoteHead _ (U u) = pure (U u)

instance (Applicative f) => AsNoteHead (->) f Beamed where
  _NoteHead = beamedNotes . traverse . _NoteHead

instance (Applicative f) => AsNoteHead (->) f [Beamed] where
  _NoteHead = traverse . _NoteHead

instance (Applicative f) => AsNoteHead (->) f Bar where
  _NoteHead f (Bar a bar) = Bar a <$> _NoteHead f bar
  _NoteHead f (PartialBar bar) = PartialBar <$> _NoteHead f bar

instance (Applicative f) => AsNoteHead (->) f Part where
  -- TODO should this touch the anacrusis?
  _NoteHead f (Part bars rep) = Part <$> (traverse . _NoteHead) f bars <*> pure rep

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

swapHands :: AsHand (->) Identity s
          => s -> s
swapHands = _Hand %~ swapH

applyMods :: [NoteMod] -> NoteHead -> NoteHead
applyMods xs a =
  foldl' (\h EndRoll -> h & noteHeadSlurEnd .~ True) a xs

instance Semigroup Beamed where
  Beamed a <> Beamed b = Beamed (a <> b)

-- instance Monoid Beamed where
--   a `mappend` b = a <> b
--   mempty = Beamed mempty
