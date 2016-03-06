{-# LANGUAGE FlexibleContexts #-}
module Score.Render (
  printScorePage, Orientation(..)
  ) where

import           Control.Lens
import qualified Data.Foldable       as F
import Control.Monad.State.Strict
import           Data.Semigroup ((<>))
import qualified Data.Music.Lilypond as L
import           Data.Ratio
import qualified Data.Sequence       as S
import           Data.VectorSpace
import           Score.Types
import           Text.Pretty


-- TODO:
-- * prettier fonts - http://lilypond.1069038.n5.nabble.com/Change-font-of-titles-composer-etc-td25870.html
-- * tweak note styles to look like this: http://drummingmad.com/what-are-unisons/
-- * dynamics
-- * staff height bigger

data Orientation = Portrait | Landscape deriving (Eq, Show)

printScorePage :: Orientation -> [Score] -> String
printScorePage o scores =
          engraverPrefix <>
          renderOrientation o <> "\n" <>
          (runPrinter . pretty . slashBlock "book" $
           slashBlock "paper" [L.Field "print-all-headers" (L.toLiteralValue "##t")] :
           slashBlock "header" [L.Field "tagline" (L.toValue "")] :
           fmap renderScore scores)

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
\startGraceMusic = {\
\  \\override NoteHead.font-size = -5\
\}\
\stopGraceMusic = {\
\  \\revert NoteHead.font-size\
\}\
\\n"

renderOrientation :: Orientation -> String
renderOrientation o' = "#(set-default-paper-size \"a4\" '" <> o <> ")"
   where o =
           case o' of
             Portrait -> "portrait"
             Landscape -> "landscape"

renderScore :: Score -> L.Music
renderScore (Score details signature ps) =
  let content =
        flip evalState [] $
        do
           bs <- traverse renderPart ps
           pure $! beginScore signature (F.toList bs)
      styles = [L.Slash1 "layout", L.Sequential [
                    L.Field "indent" (L.toLiteralValue "#0")
                   ,slashBlock "context" [
                                         L.Slash1 "Score",
                                         L.Slash1 "consists #(bars-per-line-engraver '(4))",
                                         L.Slash1 "omit BarNumber",
                                         L.Override "GraceSpacing.spacing-increment" (L.toValue (0.0::Double)),
                                         L.Field "proportionalNotationDuration" (L.toLiteralValue "#(ly:make-moment 1/8)")
                                         ]
                   ]]
      header = slashBlock "header" (renderDetails details <> [L.Field "tagline" (L.toValue "")])
   in slashBlock "score" (content ++ [header] ++ styles)

renderDetails :: Details -> [L.Music]
renderDetails ds = [
    L.Field "title"  $ L.toValue (ds ^. detailsTitle)
  , L.Field "composer"  $ L.toValue (ds ^. detailsComposer)
  , L.Field "piece"  $ L.toValue (ds ^. detailsGenre)
  ] <> maybe [] (pure . L.Field "opus" . L.toValue) (ds ^. detailsBand)

renderPart :: Part -> State [NoteMod] L.Music
renderPart p =
  do anacrusis <- renderAnacrusis (view partAnacrusis p)
     beams <- renderManyBeameds (p ^.. partBeams . traverse)
     let thisPart = L.Sequential (anacrusis ++ beams)
         r = fmap L.Sequential . renderManyBeameds . F.toList
     case p ^. partRepeat of
       NoRepeat -> pure thisPart
       Repeat -> pure $ L.Repeat False 2 thisPart Nothing
       Return (firstTime, secondTime) ->
         do ft <- restoring (r firstTime)
            st <- r secondTime
            pure $! L.Repeat False 2 thisPart (Just (ft, st))

renderAnacrusis :: Maybe Beamed -> State [NoteMod] [L.Music]
renderAnacrusis anacrusis =
     case anacrusis of
       Nothing -> pure []
       Just a ->
         do let duration = sumOf _Duration a
            bs <- renderManyBeameds (pure a)
            pure [ L.Partial (round $ 1/duration) (L.Sequential bs) ]

-- overrideValue :: Show a => String -> a -> L.Music
-- overrideValue k v = L.Override k (L.toValue v)

-- overrideLiteralValue :: String -> String -> L.Music
-- overrideLiteralValue k v = L.Override k (L.toLiteralValue v)

beginScore :: Signature -> [L.Music] -> [L.Music]
beginScore signature i =
  [L.New "Staff"
         Nothing
         (slashBlock
            "with"
            [L.Override "StaffSymbol.line-count" (L.toValue (1 :: Int))
            ,L.Override "Stem.direction" (L.toValue (-1 :: Int))
            ,L.Override "StemTremolo.beam-thickness" (L.toValue (0.3 :: Double))
            ,L.Override "StemTremolo.slope" (L.toValue (0.35 :: Double))
            -- ,L.Override "StemTremolo.Y-offset" (L.toValue ( 0.8))
            ])
  ,
   L.Sequential
     ([L.Slash1 "hide Staff.Clef",
       L.Clef L.Percussion,
       L.Slash1 "tiny"] <>
      renderSignature signature <>
      spannerStyles <>
      [beamPositions] <>
      i)]
  where -- turn text spanners into unison marks
        spannerStyles = [
          L.Override "TextSpanner.style" (L.toLiteralValue "#'line'")
         ,L.Override "TextSpanner.bound-details.left.text" $ L.toLiteralValue "\\markup { \\draw-line #'(0 . -1.5) }"
         ,L.Override "TextSpanner.bound-details.right.text" $ L.toLiteralValue "\\markup { \\draw-line #'(0 . -1.5) }"
         ,L.Override "TextSpanner.bound-details.right.padding" (L.toValue (-0.5::Double))
         ,L.Override "TextSpanner.bound-details.right.attach-dir" (L.toValue (1::Int))
         ,L.Override "TextSpanner.thickness" (L.toValue (4::Int))
         ,L.Override "TextSpanner.color" $ L.toLiteralValue "#(x11-color 'orange)"
         ,L.Override "Staff.OttavaBracket.thickness" $ L.toValue (2::Int)
         ,L.Override "Staff.OttavaBracket.color" $ L.toLiteralValue "#(x11-color 'OrangeRed)"
         ,L.Override "Staff.OttavaBracket.edge-height" $ L.toLiteralValue "#'(1.2 . 1.2)"
         ,L.Override "Staff.OttavaBracket.bracket-flare" $ L.toLiteralValue "#'(0 . 0)"
         ,L.Override "Staff.OttavaBracket.dash-fraction" $ L.toValue (1.0::Double)
         ,L.Override "Staff.OttavaBracket.shorten-pair"  $ L.toLiteralValue "#'(-0.4 . -0.4)"
         ,L.Override "Staff.OttavaBracket.staff-padding" $ L.toValue (3.0::Double)
         ,L.Override "Staff.OttavaBracket.minimum-length" $ L.toValue (1.0::Double)
           ]

setMomentAndStructure :: Integer -> [Integer] -> [L.Music]
setMomentAndStructure moment momentGroups =
  let mm = "#(ly:make-moment 1/" <> show moment <> ")"
      structure = unwords (map show momentGroups)
   in [L.Set "baseMoment" $ L.toLiteralValue mm,
       L.Set "beatStructure" $ L.toLiteralValue $ "#'(" <> structure <> ")"]

renderSignature :: Signature -> [L.Music]
renderSignature sig@(Signature n m) =
  [L.Set "strictBeatBeaming" (L.toLiteralValue "##t"),
   L.Set "subdivideBeams" $ L.toLiteralValue "##t"] <>
  momentAndStructure <>
  [L.Time n m]
  where
    momentAndStructure = case sig of
                Signature 2 4 -> setMomentAndStructure 8 [2,2,2,2]
                Signature 2 2 -> setMomentAndStructure 8 [2,2,2,2]
                Signature 3 4 -> setMomentAndStructure 8 [2, 2, 2]
                Signature 4 4 -> setMomentAndStructure 1 [4,4,4,4]
                -- TODO what does the 6 here even mean.
                Signature 6 8 -> setMomentAndStructure 6 [3, 3]
                Signature 9 8 -> setMomentAndStructure 6 [3, 3, 3]
                _ -> error "Unknown signature"

renderManyBeameds :: [Beamed] -> State [NoteMod] [L.Music]
renderManyBeameds bs =
  join <$> traverse f bs
  where f b = do notes <- resolveMods b
                 return $! renderBeamed notes

resolveMods :: Beamed -> State [NoteMod] (S.Seq Note)
resolveMods (Beamed b) =
            flip (traverse . _NoteHead) b $ \nh ->
                do mods <- get
                   let v = applyMods mods nh
                   put (nh^.noteHeadMods)
                   return (v & noteHeadMods .~ [])

renderBeamed :: S.Seq Note -> [L.Music]
renderBeamed =
    F.toList
  . buildMusic
  . addBeams
  . fmap renderNote

type PrePost = (S.Seq L.Music, S.Seq L.Music)
data RenderedNote = Graced (Maybe L.Music) PrePost (S.Seq L.Music)
                  | Tupleted Int Int (S.Seq RenderedNote)

notesOnly :: Traversal' RenderedNote L.Music
notesOnly f (Graced mb x n) = Graced mb x <$> traverse f n
notesOnly f (Tupleted n d xs) = Tupleted n d <$> (traverse . notesOnly) f xs

firstMusic :: Traversal' RenderedNote L.Music
firstMusic f (Graced mb x n) = Graced mb x <$> traverse f n
firstMusic f (Tupleted n d xs) = case F.toList xs of
                                   [] -> pure (Tupleted n d xs)
                                   (h:t) -> fmap (\x' -> Tupleted n d $ S.fromList (x':t)) (firstMusic f h)

lastMusic :: Traversal' RenderedNote L.Music
lastMusic f (Graced mb x n) = Graced mb x <$> traverse f n
lastMusic f (Tupleted n d xs) =
  let xs' = F.toList xs
   in Tupleted n d . S.fromList <$> (_last . lastMusic) f xs'

buildMusic :: S.Seq RenderedNote -> S.Seq L.Music
buildMusic rns = rns >>= f
  where f (Graced mb (pre', post) l) = pre' <> maybe l (\v -> v S.<| l ) mb <> post
        f (Tupleted n d more) =
          -- TODO: grace notes need to be rendered outside of the tuplet
          pure $
          L.Tuplet n d (L.Sequential . F.toList . buildMusic $ more)

addBeams :: S.Seq RenderedNote -> S.Seq RenderedNote
addBeams rn =
  if lengthOf (traverse . notesOnly) rn > 1
     then rn & _head . firstMusic %~ L.beginBeam
             & _last . lastMusic %~ L.endBeam
     else rn

renderNote :: Note -> RenderedNote
renderNote (Note h) = renderNoteHead h
renderNote (Rest n) = Graced Nothing mempty (pure $ L.Rest (Just $ L.Duration n) [])
renderNote (Tuplet r h) = Tupleted (fromInteger $ numerator r) (fromInteger $ denominator r) (fmap renderNote h)
renderNote (U u) = renderUnison u

renderUnison :: Unison -> RenderedNote
renderUnison u =
  let su =
          S.fromList
         [
             L.Raw "\\ottava #1"
           , L.Set "Staff.middleCPosition" (L.toValue (0::Int))
           , L.Set "Staff.ottavation" (L.toValue "")
         ]
      eu =
         S.fromList
         [
           L.Raw "\\ottava #0"
         ]
  in case u of
           StartUnison -> Graced Nothing (su, mempty) mempty
           StopUnison -> Graced Nothing (mempty, eu) mempty

renderNoteHead :: NoteHead -> RenderedNote
renderNoteHead n =
  let pitch = hand leftPitch rightPitch (n ^. noteHeadHand)
      oppPitch = hand leftPitch rightPitch (swapHands $ n ^. noteHeadHand)
      embell = fmap f (n^.noteHeadEmbellishment)
                where f Flam = slashBlock "grace" [0.5 *^ L.note (L.NotePitch oppPitch Nothing)]
                      f Drag =
                          slashBlock "grace" [
                            L.Revert "Beam.positions",
                            0.25 *^ L.note (L.NotePitch oppPitch Nothing),
                            0.25 *^ L.note (L.NotePitch oppPitch Nothing),
                            beamPositions
                            ]
                      f Ruff =
                        L.Slash1 "grace" ^+^
                         L.Sequential [
                            L.Revert "Beam.positions",
                            0.25 *^ L.note (L.NotePitch pitch Nothing),
                            0.25 *^ L.note (L.NotePitch oppPitch Nothing),
                            0.25 *^ L.note (L.NotePitch oppPitch Nothing),
                            beamPositions
                            ]
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
      finalNote =
        startTie . endTie $ thisHead

  in Graced embell mempty (pure finalNote)

leftPitch :: L.Pitch
leftPitch = L.Pitch (L.B, 0, 4)

rightPitch :: L.Pitch
rightPitch = L.Pitch (L.D, 0, 5)

beamPositions :: L.Music
beamPositions = L.Override "Beam.positions" (L.toLiteralValue "#'(-3.5 . -3.5)") 

slashBlock :: String -> [L.Music] -> L.Music
slashBlock x b = L.Slash x (L.Sequential b)

restoring :: State a b -> State a b
restoring ma =
   do s <- get
      v <- ma
      put s
      return v
