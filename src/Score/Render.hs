module Score.Render (
  printScore
  ) where

import           Control.Lens
import qualified Data.Foldable       as F
import           Data.Monoid ((<>))
import qualified Data.Music.Lilypond as L
import           Data.Ratio
import qualified Data.Sequence       as S
import           Data.VectorSpace
import           Score.Types
import           Text.Pretty

printScore :: Score -> String
printScore music = (mappend engraverPrefix $ runPrinter . pretty . renderScore $ music)

engraverPrefix :: String
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

  -- TODO anacruses, remove clef from every line, staff height bigger
  -- title/author/style
renderScore :: Score -> L.Music
renderScore (Score details signature anacrusis ps) =
  let content =
        beginScore signature ((anac ++) . F.toList . fmap renderPart $ ps)

      -- the {} from slash1/slash here is important
      styles = [L.Slash1 "layout", L.Sequential [
                    L.Field "indent" (L.toLiteralValue "#0")
                   ,L.Slash "context" (L.Sequential [
                                         L.Slash1 "Score",
                                         L.Slash1 "consists #(bars-per-line-engraver '(4))",
                                         L.Slash1 "omit BarNumber",
                                         L.Override "GraceSpacing.spacing-increment" (L.toValue (0.2::Double)),
                                         L.Field "proportionalNotationDuration" (L.toLiteralValue "#(ly:make-moment 1/8)")
          -- ,L.Override "SpacingSpanner.strict-grace-spacing" (L.toLiteralValue "##t")
          -- ,L.Override "SpacingSpanner.strict-note-spacing" (L.toLiteralValue "##t")
                                         ])]]
      anac = case anacrusis of
               Nothing -> []
               Just a -> let Sum duration = a ^. _Duration . to Sum
                          in pure $ L.Partial (round $ 1/duration) (L.Sequential $ renderBeamed a)
   in
    L.Slash "book" $ L.Sequential [
       L.Slash "header" $ L.Sequential (renderDetails details <> [L.Field "tagline" (L.toValue "")])
       , L.Slash "score" $ L.Sequential (content ++ styles)
       ]

renderDetails :: Details -> [L.Music]
renderDetails ds = [
    L.Field "title"  $ L.toValue (ds ^. detailsTitle)
  , L.Field "composer"  $ L.toValue (ds ^. detailsComposer)
  , L.Field "piece"  $ L.toValue (ds ^. detailsGenre)
  ] <> maybe [] (pure . L.Field "opus" . L.toValue) (ds ^. detailsBand)

renderPart :: Part -> L.Music
renderPart p =
  let beams = (p ^.. partBeams . traverse) >>= renderBeamed
      thisPart = L.Sequential beams
      r = L.Sequential . (=<<) renderBeamed . F.toList
   in
    case p ^. partRepeat of
      NoRepeat -> thisPart
      Repeat -> L.Repeat False 2 thisPart Nothing
      Return (firstTime, secondTime) -> L.Repeat False 2 thisPart (Just (r firstTime, r secondTime))

beginScore :: Signature -> [L.Music] -> [L.Music]
beginScore signature i =
  [
    L.New "Staff" Nothing (
      L.Slash "with" $ L.Sequential [
          L.Override "StaffSymbol.line-count" (L.toValue (1::Int))
          -- TODO: work out why this doesn't turn on bar lines at the beginning of lines
          -- ,L.Override "Score.BarLine.break-visibility" (L.toLiteralValue "#all-visible")
          ,L.Override "Stem.direction" (L.toValue (-1::Int))
          ,L.Override "StemTremolo.slope" (L.toValue (0.25::Double))
           -- ,L.Override "StemTremolo.Y-offset" (L.toValue (-0.8))
          ]
      )
  , L.Sequential ( [clefOff, L.Clef L.Percussion] <> beamStuff <> beginTime signature <> i) ]
  where clefOff = L.Slash1 "hide Staff.Clef"
        beamStuff = [beamThing] --, b0 ] --, b1, b2]
        beamThing = L.Set "strictBeatBeaming" (L.toLiteralValue "##t")
        _b0 = L.Set "subdivideBeams" $ L.toLiteralValue "##t"
        -- TODO this should be determined by the time signature
        -- For 6/8s, jigs, maybe strathspeys, I think it needs to be 1/6 or 1/12, and nfi what the beat structure
        -- is. Might need a rendering test on this once it's right
        _b1 = L.Set "baseMoment" $ L.toLiteralValue "#(ly:make-moment 1/8)"
        _b2 = L.Set "beatStructure" $ L.toLiteralValue "#'(2 2 2 2)"

beginTime :: Signature -> [L.Music]
beginTime (Signature n m) = pure $ L.Time n m

renderBeamed :: Beamed -> [L.Music]
renderBeamed =
    F.toList
  . buildMusic
  . addBeams
  . fmap renderNote
  . view _Wrapped

data RenderedNote = Graced (Maybe L.Music) L.Music
                  | Tupleted Int Int (S.Seq RenderedNote)

firstMusic :: Traversal' RenderedNote L.Music 
firstMusic f (Graced mb n) = Graced mb <$> f n
firstMusic f (Tupleted n d xs) = case F.toList xs of
                                   [] -> pure (Tupleted n d xs)
                                   (h:t) -> fmap (\x' -> Tupleted n d $ S.fromList (x':t)) (firstMusic f h)

lastMusic :: Traversal' RenderedNote L.Music 
lastMusic f (Graced mb n) = Graced mb <$> f n
lastMusic f (Tupleted n d xs) = case F.toList xs of
                                   [] -> pure (Tupleted n d xs)
                                   xs'@(_:_) ->
                                     let (h:rest) = reverse xs'
                                      in fmap (\x' -> Tupleted n d $ S.fromList $ reverse (x':rest)) (lastMusic f h)

buildMusic :: S.Seq RenderedNote -> S.Seq L.Music
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
renderNote (Rest n) = Graced Nothing (L.Rest (Just $ L.Duration n) [])
renderNote (Tuplet r (Beamed h)) = Tupleted (fromInteger $ numerator r) (fromInteger $ denominator r) (fmap renderNote h)

-- TODO: test by building pngs and comparing to known good versions
renderNoteHead :: NoteHead -> RenderedNote -- (Maybe L.Music, L.Music)
renderNoteHead n =
  let pitch = hand leftPitch rightPitch (n ^. noteHeadHand)
      oppPitch = hand leftPitch rightPitch (swapHands $ n ^. noteHeadHand)
      embell = fmap f (n^.noteHeadEmbellishment)
                where f Flam = L.Slash "grace" (0.5 *^ L.note (L.NotePitch oppPitch Nothing))
                      f Drag =
                        L.Slash1 "grace" ^+^
                      -- INFO grace note size https://lists.gnu.org/archive/html/lilypond-user/2011-04/msg00440.html
                        L.Sequential [
                            0.25 *^ L.note (L.NotePitch oppPitch Nothing),
                            0.25 *^ L.note (L.NotePitch oppPitch Nothing)
                            ]
                      f Ruff =
                        L.Slash1 "grace" ^+^
                        L.Tuplet 3 2 (L.Sequential [
                            0.25 *^ L.note (L.NotePitch pitch Nothing),
                            0.25 *^ L.note (L.NotePitch oppPitch Nothing),
                            0.25 *^ L.note (L.NotePitch oppPitch Nothing)
                            ])
      events =
        let accF =
              if n^.noteHeadAccent
                then (L.Articulation L.Above L.Accent:)
                else id
            buzzF =
              if n^.noteHeadBuzz
                 -- TODO: tremolo doesn't look quite right
                 -- I really just want to say "add 2 stripes to this stem"
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
         -- it seems grace notes are always stem up
         -- if they aren't we need this:
         -- Just e -> [L.Slash1 "stemUp", e, L.Slash1 "stemDown", finalNote]
         -- Nothing -> [L.Slash1 "stemDown", finalNote]

-- https://hackage.haskell.org/package/lilypond-1.9.0/docs/Data-Music-Lilypond.html
           -- [L.Articulation L.Default L.Accent]]-- the default note is a qtr note, so we quotient that out


leftPitch :: L.Pitch
leftPitch = L.Pitch (L.B, 0, 4)

rightPitch :: L.Pitch
rightPitch = L.Pitch (L.D, 0, 5)
