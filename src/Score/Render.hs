module Score.Render where

import           Control.Lens
import           Control.Monad       (join)
import qualified Data.Foldable       as F
import qualified Data.Music.Lilypond as L
import           Data.Ratio
import qualified Data.Sequence       as S
import Data.Sequence ((><))
import           Data.VectorSpace
import           Score.Types
import           Text.Pretty

printScore music = (mappend engraverPrefix $ runPrinter . pretty . renderScore $ music)

engraverPrefix =
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

renderScore (Score (n,m) ps) =
  L.Slash "score" $
  -- TODO Parts -- anacruses, repeats
  L.Sequential (
     beginScore n m (renderBeamed =<< (ps ^.. traverse . partBeams . traverse))
     -- the {} from slash1/slash here is important
     ++ [L.Slash1 "layout", L.Sequential $ pure (L.Slash "context" (L.Sequential [L.Slash1 "Score", L.Slash1 "consists #(bars-per-line-engraver '(4))"]))]
               )

beginScore n m i =
  [
    L.New "Staff" Nothing (
      L.Slash "with" $ L.Sequential [
          L.Override "StaffSymbol.line-count" (L.toValue (1::Int))
          ,L.Override "Stem.direction" (L.toValue (-1))
          ,L.Override "StemTremolo.slope" (L.toValue 0.25)
           -- ,L.Override "StemTremolo.Y-offset" (L.toValue (-0.8))
          ]
      )
  , L.Sequential (L.Clef L.Percussion : L.Time n m :  i) ]

renderBeamed :: Beamed -> [L.Music]
renderBeamed =
    F.toList
  . buildMusic
  . addBeams
  . fmap renderNote
  . view _Wrapped
  -- F.toList $
  -- case fromList $ F.toList (b ^. _Wrapped) of
  --   Right (Bookended a as e) -> renderNote (Just True) a >< (renderNote Nothing =<< as) >< renderNote (Just False) e
  --   Left xs -> renderNote Nothing =<< xs

data RenderedNote = Graced (Maybe L.Music) L.Music
                  | Tupleted Int Int (S.Seq RenderedNote)

firstMusic f (Graced mb n) = Graced mb <$> f n
firstMusic f (Tupleted n d xs) = case F.toList xs of
                                   [] -> pure (Tupleted n d xs)
                                   (x:xs) -> fmap (\x' -> Tupleted n d $ S.fromList (x':xs)) (firstMusic f x)

lastMusic f (Graced mb n) = Graced mb <$> f n
lastMusic f (Tupleted n d xs) = case F.toList xs of
                                   [] -> pure (Tupleted n d xs)
                                   xs@(_:_) ->
                                     let (x:rest) = reverse xs
                                      in fmap (\x' -> Tupleted n d $ S.fromList $ reverse (x':rest)) (lastMusic f x)

buildMusic rns = rns >>= f
  where f (Graced mb l) = maybe (pure l) (\v -> S.fromList [v,l]) mb
        f (Tupleted n d more) =
          pure $
          L.Tuplet n d (L.Sequential . F.toList . buildMusic $ more)

addBeams :: S.Seq RenderedNote -> S.Seq RenderedNote
addBeams rn =
  if S.length rn > 1
     then rn & _head . firstMusic %~ L.beginBeam
             & _last . lastMusic %~ L.endBeam
     else rn

renderNote :: Note -> RenderedNote
renderNote (Note h) = renderNoteHead h
renderNote (Tuplet r h) = Tupleted (fromInteger $ numerator r) (fromInteger $ denominator r) (fmap renderNote h)

x (m, i) = maybe (S.fromList [i] )(\v -> S.fromList [v, i]) m

-- TODO: test by building pngs and comparing to known good versions
renderNoteHead :: NoteHead -> RenderedNote -- (Maybe L.Music, L.Music)
renderNoteHead n =
  let pitch = hand leftPitch rightPitch (n ^. noteHeadHand)
      oppPitch = hand leftPitch rightPitch (swapHands $ n ^. noteHeadHand)
      embell = fmap f (n^.noteHeadEmbellishment)
                where f Flam = L.Slash "grace" (0.5 *^ L.note (L.NotePitch oppPitch Nothing))
                      f Drag =
                        L.Slash1 "grace" ^+^
                      -- TODO grace note size https://lists.gnu.org/archive/html/lilypond-user/2011-04/msg00440.html
                        L.Sequential [
                            0.25 *^ L.note (L.NotePitch oppPitch Nothing),
                            0.25 *^ L.note (L.NotePitch oppPitch Nothing)
                            ]
      checkBuzz = if n^.noteHeadBuzz
                then L.Tremolo 4 . (0.25*^) -- adding tremolo adds noteHead value, reduce the durations to compensate
                     -- there's a sn8:32 syntax as well (the :32), that might not add value. but im not sure
                     -- that this library supports it
                     -- TODO: tremolo doesn't look quite right
                else id
      events =
        let accF = 
              if n^.noteHeadAccent
                then (L.Articulation L.Above L.Accent:)
                else id
            buzzF =
              if n^.noteHeadBuzz
                 then (L.TremoloS 32:)
                 else id
         in accF . buzzF $ []
      thisHead = L.Note (L.NotePitch pitch Nothing) (Just $ L.Duration (n ^. noteHeadDuration)) events
      endTie = if n^.noteHeadSlurEnd
                  then L.endSlur
                  else id
      startTie = if n^.noteHeadSlurBegin
                  then L.beginSlur
                  else id
      beamed = id
        -- toggle id L.endBeam L.beginBeam beamState

      finalNote =
        beamed . startTie . endTie $ thisHead

  in Graced embell finalNote
       -- case embell of
         -- it seems grace notes are always stem up
         -- Just e -> [e, finalNote]
         -- Nothing -> [finalNote]
         -- if they aren't we need this:
         -- any grace notes need to go before the stemDown
         -- Just e -> [L.Slash1 "stemUp", e, L.Slash1 "stemDown", finalNote]
         -- Nothing -> [L.Slash1 "stemDown", finalNote]

-- https://hackage.haskell.org/package/lilypond-1.9.0/docs/Data-Music-Lilypond.html
           -- [L.Articulation L.Default L.Accent]]-- the default note is a qtr note, so we quotient that out


leftPitch :: L.Pitch
leftPitch = L.Pitch (L.B, 0, 4)

rightPitch :: L.Pitch
rightPitch = L.Pitch (L.D, 0, 5)
