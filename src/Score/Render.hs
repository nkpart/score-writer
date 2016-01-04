module Score.Render where

import           Control.Lens
import           Control.Monad       (join)
import qualified Data.Foldable       as F
import qualified Data.Music.Lilypond as L
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
     beginScore n m (join (fmap renderBeamed (ps ^.. traverse . partBeams . traverse)))
     -- the {} from slash1/slash here is important
     ++ [L.Slash1 "layout", L.Sequential $ pure (L.Slash "context" (L.Sequential [L.Slash1 "Score", L.Slash1 "consists #(bars-per-line-engraver '(4))"]))]
               )

beginScore n m i =
  [
    L.New "Staff" Nothing (
      L.Slash "with" $ L.Sequential [
          L.Override "StaffSymbol.line-count" (L.toValue (1::Int))
          ]
      )
  , L.Sequential (L.Clef L.Percussion : L.Time n m: i) ]

renderBeamed :: Beamed -> [L.Music]
renderBeamed =
    join
  . F.toList
  . fmap renderNote
  . when' ((> 1) . F.length) ((_head . _2 .~ Just True) . (_last . _2 .~ Just False))
  . fmap (\v -> (v, Nothing))
  . view _Wrapped
  where when' p f v = if p v
                         then f v
                         else v


-- TODO: test by building pngs and comparing to known good versions
renderNote :: (NoteHead, Maybe Bool) -> [L.Music]
renderNote (n, beamState) =
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
                else id
      events = []
      thisHead = checkBuzz $ L.Note (L.NotePitch pitch Nothing) (Just $ L.Duration (n ^. noteHeadDuration)) events -- TODO accents etc
      tied =
        toggle id L.endSlur L.beginSlur (n^. noteHeadSlur)
      beamed =
        toggle id L.endBeam L.beginBeam beamState

      finalNote =
        beamed . tied $ thisHead

  in
       case embell of
         -- it seems grace notes are always stem up
         Just e -> [e, finalNote]
         Nothing -> [finalNote]
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
