{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE QuasiQuotes               #-}
module Score.Render (
  printScorePage, Orientation(..), RenderingOptions(..), renderingOptionsOrientation
  ) where

import           Control.Lens hiding ((<|))
import           Control.Monad.State.Strict
import qualified Data.Foldable                as F
import           Data.List.NonEmpty           (NonEmpty((:|)), (<|))
import Score.Render.Music
import qualified Data.List.NonEmpty           as NE
import qualified Data.Music.Lilypond          as L hiding (F)
import           Data.Ratio
import           Data.Semigroup               ((<>))
import Score.Render.RenderedNote
import           Data.VectorSpace
import           Score.Types
import           Text.Pretty
import Data.String.QQ (s)

data RenderingOptions =
  RenderingOptions {_renderingOptionsOrientation :: Orientation}
  deriving (Eq,Show)

data Orientation = Portrait | Landscape deriving (Eq, Show)

makeLenses ''RenderingOptions

-- | State that needs to be managed while we render
--   * note modifications are stored here, they are picked up on one note
--     and apply to the next note
--   * bar count. used to apply a line break after every N
data RenderState =
  RenderState {_renderStateNoteMods :: [NoteMod]
              ,_renderStateBarsPerLine :: Int
              ,_renderStateBarCount :: Int}
  deriving (Eq, Show)
makeLenses ''RenderState

-- TODO:
-- * prettier fonts - http://lilypond.1069038.n5.nabble.com/Change-font-of-titles-composer-etc-td25870.html
-- * pretty note font lilyjazz http://lilypondblog.org/2016/01/arnold/#more-4291
-- * tweak note styles to look like this: http://drummingmad.com/what-are-unisons/
-- * staff height bigger


printScorePage :: RenderingOptions -> [Score] -> String
printScorePage ro scores =
          engraverPrefix <>
          renderOrientation (_renderingOptionsOrientation ro) <> "\n" <>
          (runPrinter . pretty . slashBlock "book" $
           slashBlock "paper" [L.Field "print-all-headers" (L.toLiteralValue "##t")] :
           slashBlock "header" [L.Field "tagline" (L.toValue "")] :
           fmap renderScore scores)

engraverPrefix :: String
engraverPrefix = [s|
startGraceMusic = {
  \override NoteHead.font-size = -5
}
stopGraceMusic = {
  \revert NoteHead.font-size
}
|]

renderOrientation :: Orientation -> String
renderOrientation o' = "#(set-default-paper-size \"a4" <> o <> "\")"
   where o =
           case o' of
             Portrait -> "portrait"
             Landscape -> "landscape"

renderScore :: Score -> L.Music
renderScore (Score details signature barsPerLine ps) =
  let content =
        flip evalState (RenderState [] barsPerLine 0) $
        do
           bs <- traverse renderPart ps
           pure $! beginScore signature (F.toList bs)
      styles = [L.Slash1 "layout", L.Sequential [
                    L.Field "indent" (L.toLiteralValue "#0")
                   ,slashBlock "context" [
                                         L.Slash1 "Score",
                                         L.Slash1 "omit BarNumber",
                                         L.Override "GraceSpacing.spacing-increment" (L.toValue (0::Double)),
                                         -- This should be used to give us bar lines at the start, however we
                                         -- only end up with a dot:
                                         L.Override "SystemStartBar.collapse-height" (L.toValue (1::Int)),
                                         L.Field "proportionalNotationDuration" (L.toLiteralValue "#(ly:make-moment 1/16)")
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

mark :: [L.Music]
mark =
  [L.Set "Score.markFormatter" (L.toLiteralValue "#format-mark-box-numbers")
  ,L.Raw "\\mark \\default"]

renderPart :: Part -> State RenderState L.Music
renderPart p =
  do let (anacrusis, rest) = case p ^. partBars of
                               a@(PartialBar _):xs -> ([a], xs)
                               xs -> ([], xs)
     ana <- renderBars anacrusis
     beams <- renderBars rest
     let thisPart =
           L.Sequential (ana <> mark <> F.toList beams)
         r =
           fmap (L.Sequential . F.toList . join) .
           traverse renderBar .
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

renderBars :: [Bar] -> State RenderState [L.Music]
renderBars = fmap join . traverse renderBar

renderBar :: Bar -> State RenderState [L.Music]
renderBar (SignatureChange x) = pure (renderSignature x)
renderBar (PartialBar b) = renderAnacrusis b <&> (<> [L.Raw "|", L.Slash1 "noBreak"])
renderBar (Bar bs) =
  do let Sum barLength = bs ^. _Duration . to Sum
     -- render barLength / min-note-dur hidden spacing notes
         _ys = (round $ barLength / (1 % 32)) :: Int
     barsPerLine <- use renderStateBarsPerLine
     v <- renderManyBeameds bs
     -- let v' = L.Sequential (L.Slash1 "oneVoice" :v) `ggg` L.Sequential [L.Repeat True _ys (L.Sequential [L.Slash1 "hideNotes", L.Raw "c''32" ]) Nothing]
     let v' = v
         -- ggg a b = L.simultaneous b a
     newCount <- renderStateBarCount <+= 1
     return $
               if newCount `mod` barsPerLine == 0
                then v' <> [L.Raw "|", L.Slash1 "break"]
                else v' <> [L.Raw "|", L.Slash1 "noBreak"]

renderAnacrusis :: Beamed -> State RenderState [L.Music]
renderAnacrusis a =
         do let duration = sumOf _Duration a
            bs <- renderManyBeameds (pure a)
            pure [ L.Partial (round $ 1/duration) (L.Sequential $ F.toList bs) ]

beginScore :: Signature -> [L.Music] -> [L.Music]
beginScore signature i =
  [L.New "Staff"
         Nothing
         (slashBlock
            "with"
            [override "StaffSymbol.line-count" (1 :: Int)
            ,override "Stem.direction" (-1 :: Int)
            -- TODO play with beam thickness
            -- ,L.Override "Beam.beam-thickness" (L.toValue (0.4 :: Double))
            ,override "StemTremolo.beam-thickness" (0.3 :: Double)
            ,override "StemTremolo.slope" (0.35 :: Double)
            ])
  ,
   L.Sequential
     ([
       L.Slash1 "hide Staff.Clef",
       L.Clef L.Percussion,
       L.Slash1 "tiny"] <>
      spannerStyles <>
      [beamPositions] <>
      renderSignature signature <>
      i)
  ]
  where -- turn ottava brackets (octave switchers) into Unison marks
        spannerStyles = [
          override "DynamicLineSpanner.direction" (1::Int)
         ,override "Staff.OttavaBracket.thickness" (2::Int)
         ,overrideL "Staff.OttavaBracket.color" "#(x11-color 'OrangeRed)"
         ,overrideL "Staff.OttavaBracket.edge-height" "#'(1.2 . 1.2)"
         ,overrideL "Staff.OttavaBracket.bracket-flare" "#'(0 . 0)"
         ,override "Staff.OttavaBracket.dash-fraction" (1.0::Double)
         ,overrideL "Staff.OttavaBracket.shorten-pair" "#'(-0.4 . -0.4)"
         ,override "Staff.OttavaBracket.staff-padding" (3.0::Double)
         ,override "Staff.OttavaBracket.minimum-length" (1.0::Double)
         ]

setMomentAndStructure :: Integer -> [Integer] -> [L.Music]
setMomentAndStructure moment momentGroups =
  let mm = "#(ly:make-moment 1/" <> show moment <> ")"
      structure = unwords (map show momentGroups)
   in literalSet "baseMoment" mm <>
      literalSet "beatStructure" ("#'(" <> structure <> ")")

literalSet :: Applicative f => String -> String -> f L.Music
literalSet k v = pure (L.Set k (L.toLiteralValue v))

renderSignature :: Signature -> [L.Music]
renderSignature sig@(Signature n m) =
  literalSet "strictBeatBeaming" "##t" <>
  literalSet "subdivideBeams" "##t" <>
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

renderManyBeameds :: [Beamed] -> State RenderState [L.Music]
renderManyBeameds bs =
  (join . fmap F.toList) <$> traverse f bs
  where f b = do notes <- resolveMods b
                 return $! renderBeamed notes

resolveMods :: MonadState RenderState f => Beamed -> f (NonEmpty Note)
resolveMods (Beamed b) =
            forOf (traverse . _NoteHead) b $ \nh ->
                do mods <- use renderStateNoteMods
                   let v = applyMods mods nh
                   renderStateNoteMods .= (nh^.noteHeadMods)
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

renderRest :: Duration -> RenderedNote
renderRest n = OtherMusic (pure $ L.Rest (Just $ L.Duration n) [])

restoring :: State a b -> State a b
restoring ma =
   do x <- get
      v <- ma
      put x
      return v
