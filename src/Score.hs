{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE OverloadedLists              #-}
module Score where

import           Control.Lens
import qualified Data.Foldable       as F
import Control.Monad (join)
import qualified Data.Music.Lilypond as L
import           Data.Ratio
import           Data.Sequence
import           Data.VectorSpace
import           System.Process
import           Text.Pretty

leftPitch :: L.Pitch
leftPitch = L.Pitch (L.B, 0, 4)

rightPitch :: L.Pitch
rightPitch = L.Pitch (L.D, 0, 5)
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

data Note =
  Note {_noteHand :: Hand
       ,_noteAccent :: Bool
       ,_noteBuzz :: Bool
       ,_noteDuration :: Ratio Integer
       ,_noteSlur :: Maybe Bool
       ,_noteEmbellishment :: Maybe Embellishment}
  deriving (Eq,Show)

newtype Beamed =
  Beamed (Seq Note)
  deriving (Eq,Show)

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
makeLenses ''Note
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

instance (Choice p, Applicative f) => AsHand p f L.Note where
  _Hand = prism f g
    where f L = L.NotePitch leftPitch Nothing
          f R = L.NotePitch rightPitch Nothing
          g (L.NotePitch (L.Pitch (L.B, _, _)) _) = Right L
          g (L.NotePitch (L.Pitch (L.D, _, _)) _) = Right R
          g v = Left v

class AsDuration p f s where
  _Duration ::
    Optic' p f s (Ratio Integer)

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

instance (Applicative f) => AsNote (->) f Part where
  _Note f (Part rep x ph) = Part rep x <$> (traverse . _Note) f ph

instance (Applicative f) => AsNote (->) f Score where
  _Note f (Score sig s) = Score sig <$> (traverse . _Note) f s

-- Utilities

swapH :: Hand -> Hand
swapH L = R
swapH R = L

_swapH :: Iso' Hand Hand
_swapH = iso swapH swapH

swapHands :: AsHand (->) Identity s
          => s -> s
swapHands = _Hand %~ swapH

renderBeamed :: Beamed -> L.Music
renderBeamed =
  L.Sequential
  . join
  . F.toList
  . fmap renderNote
  . when' ((> 1) . F.length) ((_head . _2 .~ Just True) . (_last . _2 .~ Just False))
  . fmap (\v -> (v, Nothing))
  . view _Wrapped
  where when' p f v = if p v
                         then f v
                         else v

type Toggle = Maybe Bool

toggle :: a -> a -> a -> Toggle -> a
toggle f g h = maybe f (\v -> bool v g h)
  where bool x a b = if x then b else a

renderNote :: (Note, Maybe Bool) -> [L.Music]
renderNote (n, beamState) =
  let pitch = hand leftPitch rightPitch (n ^. noteHand)
      oppPitch = hand leftPitch rightPitch (swapHands $ n ^. noteHand)
      embell = fmap f (n^.noteEmbellishment)
                where f Flam = L.Slash "grace" (0.5 *^ L.note (L.NotePitch oppPitch Nothing))
                      f Drag =
                        L.Slash1 "grace" ^+^
                      -- TODO grace note size https://lists.gnu.org/archive/html/lilypond-user/2011-04/msg00440.html
                        L.Sequential [
                            0.25 *^ L.note (L.NotePitch oppPitch Nothing),
                            0.25 *^ L.note (L.NotePitch oppPitch Nothing)
                            ]
      checkBuzz = if n^.noteBuzz
                then L.Tremolo 4 . (0.25*^) -- adding tremolo adds note value, reduce the durations to compensate
                     -- there's a sn8:32 syntax as well (the :32), that might not add value. but im not sure
                     -- that this library supports it
                else id
      events = []
      thisHead = checkBuzz $ L.Note (L.NotePitch pitch Nothing) (Just $ L.Duration (n ^. noteDuration)) events -- TODO accents etc
      tied =
        toggle id L.endSlur L.beginSlur (n^. noteSlur)
      beamed =
        toggle id L.endBeam L.beginBeam beamState

      finalNote =
        beamed . tied $ thisHead

  in
       case embell of
         -- any grace notes need to go before the stemDown
         Just e -> [L.Slash1 "stemUp", e, L.Slash1 "stemDown", finalNote]
         Nothing -> [L.Slash1 "stemDown", finalNote]

-- https://hackage.haskell.org/package/lilypond-1.9.0/docs/Data-Music-Lilypond.html
           -- [L.Articulation L.Default L.Accent]]-- the default note is a qtr note, so we quotient that out

beginScore n m =
  -- L.Slash "layout" (L.Slash "context" (L.Sequential [L.Slash1 "Score", L.Slash1 "consists #(bars-per-line-engraver '(4))"])) ^+^
  L.Sequential [
    L.New "Staff" Nothing (
      L.Slash "with" $ L.Sequential [
          L.Override "StaffSymbol.line-count" (L.toValue (1::Int))

          ]
      )
  , L.Clef L.Percussion
  , L.Time n m]


tidy (L.Sequential [x]) = tidy x
tidy x = x

beamOf = renderBeamed . Beamed

renderScore (Score (n,m) ps) =
  L.Sequential (
  -- TODO Parts -- anacruses, repeats
  -- measures per line
  beginScore n m : fmap renderBeamed (ps ^.. traverse . partBeams . traverse) )

someFunc :: IO ()
someFunc =
  do let music = Score (4,4) . pure . Part NoRepeat Nothing $
                 [  Beamed [rFlam (1/4)]
                  , Beamed [lFlam (1/8), rn (1/8) & buzz & start noteSlur]
                  , Beamed [rn (1/8) & end noteSlur, ln (1/8)]
                  , Beamed [rn (1/8) & buzz & accent & start noteSlur, rn (1/8) & accent & end noteSlur]
                  ,
                      singles4Qtr & _Note . noteDuration .~ (1/16)
                                  & elementOf _Note 0 %~ buzz . start noteSlur
                                  & elementOf _Note 1 %~ ((noteHand .~ R) . end noteSlur )
                                  & elementOf _Note 2 . noteEmbellishment .~ Just Drag
                  ,
                      singles4Qtr & _Note . noteDuration .~ (1/16)
                                  & elementOf _Note 0 . noteBuzz .~ False
                                  & elementOf _Note 2 . noteEmbellishment .~ Just Drag
                  ]

     let prefix =
           "\
\#(define ((bars-per-line-engraver bar-list) context) \n\
\  (let* ((working-copy bar-list)\n\
\         (total (1+ (car working-copy))))\n\
\    `((acknowledgers\n\
\       (paper-column-interface\n\
\        . ,(lambda (engraver grob source-engraver)\n\
\             (let ((internal-bar (ly:context-property context 'internalBarNumber)))\n\
\               (if (and (pair? working-copy)\n\
\                        (= (remainder internal-bar total) 0)\n\
\                        (eq? #t (ly:grob-property grob 'non-musical)))\n\
\                   (begin\n\
\                     (set! (ly:grob-property grob 'line-break-permission) 'force)\n\
\                     (if (null? (cdr working-copy))\n\
\                         (set! working-copy bar-list)\n\
\                         (begin\n\
\                           (set! working-copy (cdr working-copy))))\n\
\                           (set! total (+ total (car working-copy))))))))))))\n\
\\n"

     writeFile "test.ly" (mappend prefix $ runPrinter . pretty . renderScore $ music)
     callCommand "lilypond test.ly"
     callCommand "open -a Safari -g test.pdf"

     -- these are not the same
     -- let on g a b =
     --       do print (g a)
     --          print (g b)
     -- on (runPrinter . pretty) (L.Slash "x" $ L.Time 2 4) (L.Slash1 "x" ^+^ L.Time 2 4)

-- BITS

singles4Qtr :: Beamed
singles4Qtr = Beamed (fromList [n R,n L,n R,n L])
         where n h = aNote h (1/4)

lFlam d = aNote L d & noteEmbellishment .~ Just Flam

rFlam d = aNote R d & noteEmbellishment .~ Just Flam

ln d = aNote L d
rn d = aNote R d

flam = noteEmbellishment .~ Just Flam

drag = noteEmbellishment .~ Just Drag

accent = noteAccent .~ True

buzz = noteBuzz .~ True

start l = l .~ Just True

end l = l .~ Just False

clear l = l .~ Nothing

aNote :: Hand -> Ratio Integer -> Note
aNote h d = Note h False False d Nothing Nothing
