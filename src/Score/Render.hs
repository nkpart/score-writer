{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
module Score.Render (
  printScorePage, Orientation(..)
  ) where

import           Control.Lens
import           Control.Monad.State.Strict
import qualified Data.Foldable                as F
import           Data.List.NonEmpty           (NonEmpty((:|)))
import qualified Data.List.NonEmpty           as NE
import qualified Data.Music.Lilypond          as L hiding (F)
import qualified Data.Music.Lilypond.Dynamics as L
import           Data.Ratio
import           Data.Semigroup               ((<>))
import           Data.VectorSpace
import           Score.Types
import           Text.Pretty

data RenderedNote
  =
    -- This lets us add beams just to these bits of music, otherwise
    -- we might attach them to things that aren't notes (overrides, etc.)
    NoteMusic L.Music
  |
    -- We track grace notes because they need to be floated outside of
    -- tuplets
    GraceMusic (NE.NonEmpty L.Music)
    -- Any other bits of lilypond data (overrides, etc.) falls here
  | OtherMusic (NE.NonEmpty L.Music)
    -- Tuplets need to be rendered as a block, so they are recursive
  | Tupleted Int
             Int
             (NE.NonEmpty RenderedNote)

-- | _NoteMusic :: Traversal' RenderedNote L.Music
_NoteMusic :: (Applicative f,Functor f) => (L.Music -> f L.Music) -> RenderedNote -> f RenderedNote
_NoteMusic f (NoteMusic c) = NoteMusic <$> f c
_NoteMusic _ (GraceMusic c) = pure (GraceMusic c)
_NoteMusic _ (OtherMusic c) = pure (OtherMusic c)
_NoteMusic f (Tupleted a b c) = Tupleted a b <$> (traverse . _NoteMusic) f c

-- TODO:
-- * prettier fonts - http://lilypond.1069038.n5.nabble.com/Change-font-of-titles-composer-etc-td25870.html
-- * tweak note styles to look like this: http://drummingmad.com/what-are-unisons/
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
renderOrientation o' = "#(set-default-paper-size \"a4" <> o <> "\")"
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
                                         L.Override "GraceSpacing.spacing-increment" (L.toValue (0::Double)),
                                         -- This should be used to give us bar lines at the start, however we
                                         -- only end up with a dot:
                                         L.Override "SystemStartBar.collapse-height" (L.toValue (1::Int)),
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
  do anacrusis <-
       renderAnacrusis (p ^. partAnacrusis)
     let mark =
           [L.Set "Score.markFormatter" (L.toLiteralValue "#format-mark-box-numbers")
           ,L.Raw "\\mark \\default"]
     beams <-
       renderManyBeameds (p ^.. partBeams . traverse)
     let thisPart =
           L.Sequential (anacrusis <> mark <> F.toList beams)
         r =
           fmap (L.Sequential . F.toList) .
           renderManyBeameds .
           F.toList
     case p ^. partRepeat of
       NoRepeat -> pure thisPart
       Repeat ->
         pure (L.Repeat False 2 thisPart Nothing)
       Return firstTime secondTime ->
         do
            -- we want to pass the same mods into the first and second time, I think
            ft <- restoring (r firstTime)
            st <- r secondTime
            pure (L.Repeat False 2 thisPart (Just (ft,st)))

renderAnacrusis :: Maybe Beamed -> State [NoteMod] [L.Music]
renderAnacrusis anacrusis =
     case anacrusis of
       Nothing -> pure []
       Just a ->
         do let duration = sumOf _Duration a
            bs <- renderManyBeameds (pure a)
            pure [ L.Partial (round $ 1/duration) (L.Sequential $ F.toList bs) ]

beginScore :: Signature -> [L.Music] -> [L.Music]
beginScore signature i =
  [L.New "Staff"
         Nothing
         (slashBlock
            "with"
            [L.Override "StaffSymbol.line-count" (L.toValue (1 :: Int))
            ,L.Override "Stem.direction" (L.toValue (-1 :: Int))
            -- TODO play with beam thickness
            -- ,L.Override "Beam.beam-thickness" (L.toValue (0.4 :: Double))
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
  where -- turn text spanners into Unison marks
        spannerStyles = [
          L.Override "TextSpanner.style" (L.toLiteralValue "#'line'")
         ,L.Override "TextSpanner.bound-details.left.text" $ L.toLiteralValue "\\markup { \\draw-line #'(0 . -1.5) }"
         ,L.Override "TextSpanner.bound-details.right.text" $ L.toLiteralValue "\\markup { \\draw-line #'(0 . -1.5) }"
         ,L.Override "TextSpanner.bound-details.right.padding" (L.toValue (-0.5::Double))
         ,L.Override "TextSpanner.bound-details.right.attach-dir" (L.toValue (1::Int))
         ,L.Override "TextSpanner.thickness" (L.toValue (4::Int))
         ,L.Override "TextSpanner.color" $ L.toLiteralValue "#(x11-color 'orange)"
         ,L.Override "DynamicLineSpanner.direction" $ L.toValue (1::Int)
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
                Signature 12 8 -> setMomentAndStructure 6 [3, 3, 3, 3]
                _ -> error "Unknown signature"

renderManyBeameds :: [Beamed] -> State [NoteMod] [L.Music]
renderManyBeameds bs =
  (join . fmap F.toList) <$> traverse f bs
  where f b = do notes <- resolveMods b
                 return $! renderBeamed notes

resolveMods :: MonadState [NoteMod] m => Beamed -> m (NonEmpty Note)
resolveMods (Beamed b) =
            forOf (traverse . _NoteHead) b $ \nh ->
                do mods <- get
                   let v = applyMods mods nh
                   put (nh^.noteHeadMods)
                   return (v & noteHeadMods .~ [])

renderBeamed :: NonEmpty Note -> NonEmpty L.Music
renderBeamed =
    buildMusic
  . addBeams
  . (>>= renderNote)

addBeams :: Traversable t => t RenderedNote -> t RenderedNote
addBeams rn =
  let numNotes = lengthOf visitNotes rn
      visitNotes = traverse . _NoteMusic
  in if numNotes > 1
        then rn & taking 1 visitNotes %~ L.beginBeam
                & dropping (numNotes - 1) visitNotes %~ L.endBeam
        else rn

buildMusic :: NonEmpty RenderedNote -> NonEmpty L.Music
buildMusic rns = rns >>= f
  where f (NoteMusic l) = pure l
        f (GraceMusic l) = l
        f (OtherMusic l) = l
        f (Tupleted n d more) =
          pure $
          L.Tuplet n d (L.Sequential . F.toList . buildMusic $ more)

renderNote :: Note -> NE.NonEmpty RenderedNote
renderNote (Note h) = renderNoteHead h
renderNote (Rest n) = pure (renderRest n)
renderNote (Tuplet r h) =
  let notes = renderNote =<< h
      tupletGroup = Tupleted (fromInteger $ numerator r) (fromInteger $ denominator r)
      mkTuplet ns =
        case ns of
          GraceMusic o :| (x:xs) -> GraceMusic o <| mkTuplet (x:|xs)
          _ -> pure $ tupletGroup ns
   in mkTuplet notes
renderNote (U u) = pure $ renderUnison u

renderRest :: Duration -> RenderedNote
renderRest n = OtherMusic (pure $ L.Rest (Just $ L.Duration n) [])

renderUnison :: Unison -> RenderedNote
renderUnison u =
  let su =
         L.Raw "\\ottava #1"
         :|
         [L.Set "Staff.middleCPosition" (L.toValue (0::Int))
         ,L.Set "Staff.ottavation" (L.toValue "")
         ]
      eu =
         pure (L.Raw "\\ottava #0")
  in case u of
           StartUnison -> GraceMusic su
           StopUnison -> GraceMusic eu

tweaksForBigAccent :: NE.NonEmpty L.Music
tweaksForBigAccent =
         L.Raw "\\once \\override Script.rotation = #'(-90 0 0)"
         :|
         [L.Raw "\\once \\override Script.font-size = #3"
         ,L.Raw "\\once \\override Script.staff-padding = #2.0"]

revertsForBigAccent :: NE.NonEmpty L.Music
revertsForBigAccent =
   L.Revert "Script.rotation" :| L.Revert "Script.font-size" : L.Revert "Script.staff-padding" : []

renderNoteHead :: NoteHead -> NE.NonEmpty RenderedNote
renderNoteHead n =
  let pitch = hand leftPitch rightPitch (n ^. noteHeadHand)
      oppPitch = hand rightPitch leftPitch (n ^. noteHeadHand)
      embell = fmap (GraceMusic . f) (n^.noteHeadEmbellishment)
                where f Flam = pure $ slashBlock "slashedGrace" [0.5 *^ L.note (L.NotePitch oppPitch Nothing)]
                      f Drag =
                            pure $
                            slashBlock "grace" [
                              L.Revert "Beam.positions",
                              0.25 *^ L.note (L.NotePitch oppPitch Nothing),
                              0.25 *^ L.note (L.NotePitch oppPitch Nothing),
                              beamPositions
                              ]
                      f Ratamacue =
                            pure $
                            slashBlock "grace" [
                              0.25 *^ L.note (L.NotePitch oppPitch Nothing),
                              0.25 *^ L.note (L.NotePitch oppPitch Nothing)
                              ]
                      f Ruff = pure $
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
              case n^.noteHeadAccent of
                AccentBig -> (L.Articulation L.Above L.Accent:)
                AccentRegular -> (L.Articulation L.Above L.Accent:)
                NoAccent -> id
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
      addDynamics = case n^.noteHeadDynamics of
                      Just p -> L.addDynamics' L.Above (mapDynamics p)
                      Nothing -> id
      addCresc = case n^.noteHeadCrescBegin of
                   Just Cresc -> L.beginCresc
                   Just Decresc -> L.beginDim
                   Nothing -> id
      stopCresc = if n^.noteHeadCrescEnd
                   then L.endCresc
                   else id
      preMusic = case n ^. noteHeadAccent of
                   NoAccent -> []
                   AccentRegular -> []
                   AccentBig -> pure (OtherMusic tweaksForBigAccent)
      postMusic = case n ^. noteHeadAccent of
                   NoAccent -> []
                   AccentRegular -> []
                   AccentBig -> pure (OtherMusic revertsForBigAccent)
      finalNote =
        startTie . endTie . addDynamics . addCresc . stopCresc $ thisHead
  -- TODO refactor fromList. might need some more NE methods (preppend and append foldables of a)
  in maybe id cons embell $ NE.fromList $ preMusic <> [NoteMusic finalNote] <> postMusic

mapDynamics :: Dynamics -> L.Dynamics
mapDynamics p = case p of
                  PPPPP -> L.PPPPP
                  PPPP -> L.PPPP
                  PPP -> L.PPP
                  PP -> L.PP
                  P -> L.P
                  MP -> L.MP
                  MF -> L.MF
                  F -> L.F
                  FF -> L.FF
                  FFF -> L.FFF
                  FFFF -> L.FFFF
                  SF -> L.SF
                  SFF -> L.SFF
                  SFZ -> L.SFZ
                  RFZ -> L.RFZ
                  SP -> L.SP
                  SPP -> L.SPP

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
