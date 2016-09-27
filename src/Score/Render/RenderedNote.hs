module Score.Render.RenderedNote where

import           Control.Lens hiding ((<|))
import qualified Data.Foldable as F
import           Data.List.NonEmpty           (NonEmpty((:|)))
import qualified Data.List.NonEmpty           as NE
import qualified Data.Music.Lilypond          as L hiding (F)
import qualified Data.Music.Lilypond.Dynamics as L
import           Data.Semigroup               ((<>))
import           Data.VectorSpace
import           Score.Types
import Score.Render.Music

-- | Annotated productions of Music
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
_NoteMusic :: (Applicative f) => (L.Music -> f L.Music) -> RenderedNote -> f RenderedNote
_NoteMusic f (NoteMusic c) = NoteMusic <$> f c
_NoteMusic _ (GraceMusic c) = pure (GraceMusic c)
_NoteMusic _ (OtherMusic c) = pure (OtherMusic c)
_NoteMusic f (Tupleted a b c) = Tupleted a b <$> (traverse . _NoteMusic) f c

renderNoteHead :: NoteHead -> NE.NonEmpty RenderedNote
renderNoteHead n =
  let pitch = hand leftPitch rightPitch (n ^. noteHeadHand)
      oppPitch = hand rightPitch leftPitch (n ^. noteHeadHand)
      embell = fmap f (n^.noteHeadEmbellishment)
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
      unisonMusic =
                 case n ^. noteHeadStartUnison of
                   False -> []
                   True -> F.toList renderStartUnison

      postMusic = case n ^. noteHeadAccent of
                   NoAccent -> []
                   AccentRegular -> []
                   AccentBig -> pure (OtherMusic revertsForBigAccent)
              <> case n ^. noteHeadStopUnison of
                   False -> []
                   True -> [OtherMusic renderStopUnison]
      finalNote =
        startTie . endTie . addDynamics . addCresc . stopCresc $ thisHead
  -- TODO refactor fromList. might need some more NE methods (preppend and append foldables of a)
      a `pref` b =
        NE.fromList (F.toList a <> NE.toList b)
  in
    NE.fromList $
    maybe (F.toList $ fmap OtherMusic $ NE.nonEmpty unisonMusic) (\a -> [GraceMusic $ unisonMusic `pref` a]) embell <> preMusic <> [NoteMusic finalNote] <> postMusic

leftPitch :: L.Pitch
leftPitch = L.Pitch (L.B, 0, 4)

rightPitch :: L.Pitch
rightPitch = L.Pitch (L.D, 0, 5)

slashBlock :: String -> [L.Music] -> L.Music
slashBlock x b = L.Slash x (L.Sequential b)

beamPositions :: L.Music
beamPositions = overrideL "Beam.positions" "#'(-3.5 . -3.5)"

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

renderStopUnison :: Applicative f => f L.Music
renderStopUnison =
         pure (L.Raw "\\ottava #0")

tweaksForBigAccent :: NE.NonEmpty L.Music
tweaksForBigAccent =
         L.Raw "\\once \\override Script.rotation = #'(-90 0 0)"
         :|
         [L.Raw "\\once \\override Script.font-size = #3"
         ,L.Raw "\\once \\override Script.staff-padding = #2.0"]

revertsForBigAccent :: NE.NonEmpty L.Music
revertsForBigAccent =
   L.Revert "Script.rotation" :| L.Revert "Script.font-size" : L.Revert "Script.staff-padding" : []


renderStartUnison :: NonEmpty L.Music
renderStartUnison =
         L.Raw "\\ottava #1"
         :|
         [L.Set "Staff.middleCPosition" (L.toValue (0::Int))
         ,L.Set "Staff.ottavation" (L.toValue "")
         ]
