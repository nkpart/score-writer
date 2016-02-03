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

data Orientation = Portrait | Landscape deriving (Eq, Show)

renderOrientation :: Orientation -> String
renderOrientation o' =
        "#(set-default-paper-size \"a4\" '" <> o <> ")"
        where o = case o' of
                    Portrait -> "portrait"
                    Landscape -> "landscape"

printScorePage :: Orientation -> [Score] -> String
printScorePage o scores = mappend engraverPrefix stuff
  where -- _stuff' = runPrinter . pretty . L.Slash "book" . L.Sequential $ List.intercalate [L.Slash1 "pageBreak"] (fmap renderPage scores)
        stuff = renderOrientation o <> "\n" <> (runPrinter . pretty . L.Slash "book" . L.Sequential $ paperBlock : headerBlock : fmap renderScore scores)
        paperBlock = L.Slash "paper" (L.Sequential [L.Field "print-all-headers" (L.toLiteralValue "##t")])
        headerBlock = L.Slash "header" (L.Sequential [L.Field "tagline" (L.toValue "")])

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

  -- TODO staff height bigger, grace notes smaller
  -- title/author/style
renderScore :: Score -> L.Music
renderScore (Score details signature ps) =
  let content =
        flip evalState [] $
        do
           bs <- traverse renderPart ps
           pure $! beginScore signature (F.toList bs)

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
   in
    let header = L.Slash "header" $ L.Sequential (renderDetails details <> [L.Field "tagline" (L.toValue "")])
    in L.Slash "score" $ L.Sequential (content ++ [header] ++ styles)

renderAnacrusis :: Maybe Beamed -> State [NoteMod] [L.Music]
renderAnacrusis anacrusis =
    case anacrusis of
      Nothing -> pure []
      Just a ->
        do let duration = sumOf _Duration a
           bs <- renderManyBeameds (pure a)
           pure $! [ L.Partial (round $ 1/duration) (L.Sequential bs) ]

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
            -- a split bar
            -- st <- L.Partial (round $ 1 / (sumOf (traverse . _Duration) secondTime)) <$> r secondTime
            st <- r secondTime
            pure $! L.Repeat False 2 thisPart (Just (ft, st))

restoring :: State a b -> State a b
restoring ma =
  do s <- get
     v <- ma
     put s
     return v

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
  , L.Sequential ( [clefOff, L.Clef L.Percussion] <> beginTime signature <> i) ]
  where clefOff = L.Slash1 "hide Staff.Clef"

setMomentAndStructure :: Integer -> [Integer] -> [L.Music]
setMomentAndStructure moment momentGroups =
  let mm = "#(ly:make-moment 1/" <> show moment <> ")"
      structure = unwords (map show momentGroups)
   in [L.Set "baseMoment" $ L.toLiteralValue mm,
       L.Set "beatStructure" $ L.toLiteralValue $ "#'(" <> structure <> ")"]

beginTime :: Signature -> [L.Music]
beginTime sig@(Signature n m) = beamStuff <> [L.Time n m]
  where
        beamStuff = [L.Set "strictBeatBeaming" (L.toLiteralValue "##t"),
                     L.Set "subdivideBeams" $ L.toLiteralValue "##t"] <> _bx
        _bx = case sig of
                -- TODO: Flesh out more signatures
                -- TODO: rendering test of beaming in strathspeys, jigs, 3/4s, etc.
                Signature 2 4 -> setMomentAndStructure 8 [2,2,2,2]
                Signature 2 2 -> setMomentAndStructure 8 [2,2,2,2]
                -- TODO what does the 6 here even mean.
                Signature 6 8 -> setMomentAndStructure 6 [3, 3]
                _ -> error "Unknown signature"

renderManyBeameds :: [Beamed] -> State [NoteMod] [L.Music]
renderManyBeameds bs =
  join <$> traverse f bs
  where f b = do notes <- resolveMods b
                 return $! renderBeamed notes

resolveMods :: Beamed -> State [NoteMod] (S.Seq Note)
resolveMods b =
          do mods <- get
             let Beamed notes leftoverMods = Beamed mempty mods <> b
             put leftoverMods
             return notes

renderBeamed :: S.Seq Note -> [L.Music]
renderBeamed =
    F.toList
  . buildMusic
  . addBeams
  . fmap renderNote

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
          -- TODO: grace notes need to be rendered outside of the tuplet
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
renderNote (Tuplet r h) = Tupleted (fromInteger $ numerator r) (fromInteger $ denominator r) (fmap renderNote h)

renderNoteHead :: NoteHead -> RenderedNote
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

leftPitch :: L.Pitch
leftPitch = L.Pitch (L.B, 0, 4)

rightPitch :: L.Pitch
rightPitch = L.Pitch (L.D, 0, 5)
