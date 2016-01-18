{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE NoMonomorphismRestriction          #-}
module Score.Library.Solos where

import Score.Prelude hiding (dot, cut)
import qualified Score.Prelude as P (dot, cut)

jimmyBlue :: Score
jimmyBlue =
  Score (Details "Jimmy Blue"
                 "Hornpipe"
                 "Arthur Cook"
                 Nothing)
        (Signature 2 4)
        Nothing [p1, p2, p3, p4]
  where
    p1 = buildPart $
      do let commonStart = bars
              [r16 & flam <-> ldrag <-> dotCut (l16 <-> r16 & accent), dotCut (l16<->l16<->r16&drag<->r16&accent)
              ,dotCut (l16<->r16<->l16&drag<->l16 & accent), singles 4 r32 <-> dotCut (r16 & accent <-> l16)
              ]
         commonStart
         bars [dotCut (h2h r16 [flam,id,flam,flam]), singles 4 r32 <-> singles 4 r32 & accentFirst
              ,r8&accent.roll<->l8&accent.roll.endRoll, dotCut (dbl r16 [accent.endRoll.roll, accent.endRoll, accent.roll, accent.endRoll])]
         commonStart
         bars ending2
         thenRepeat

    p2 = buildPart $
      do let commonStart =
              let b1 = [ para r32&zapN 0 flam <-> dotCut (l16 <-> l16 & flam) , dotCut (r16 <-> r16 & flam <-> l16 <-> l16 & flam )  ]
               in b1 <> fmap swapHands b1
         bars commonStart
         bars [para r32&zapN 0 flam<->dotCut (l16<->l16&flam), singles 4 r32 <-> r8&accent.roll
              ,dotCut(l16&endRoll.roll.accent<->l16&endRoll.accent)<->singles 4 r32, r8&accent<->dotCut(r16&flam<->r16&flam)]
         let
           return2 = -- the start of this movement should be dot/cut in both rounded and swung
             let v = r16&P.dot.accent<->l16&P.cut.accent<->singles 4 r32&accentN 3<->singles 4 r32&accentFirst<->singles 4 r32&accentFirst
              in [v,v]
         firstTime $ commonStart <> ending2
         secondTime $ return2 <> ending2

    --- PART 3
    p3 = buildPart $ bars (first2 <> end2' <> first2 <> ending2) >> thenRepeat
      where first2 =
              [dotCut (r16&flam<->l16&roll<->l16&endRoll<->r16&roll), dotCut(r16&endRoll<->l16<->r16<->l16&flam)
              ] <> (let v = r8&flam.roll<->dotCut (l16&endRoll<->r16) in [v, swapHands v])
            end2' =
              [dotCut (r16&flam<->l16<->r16&flam<->l16&flam), singles 4 r32 & accentN 3 <-> singles 4 r32
              ,dotCut (r16&drag<->r16&accent<->l16<->r16&drag), singles 4 r32 <-> dotCut (r16&accent<->l16)]

    --- PART 4
    p4 = buildPart $ bars firstLine >> firstTime (second2 <> ending2) >> secondTime (return2 <> ending2)
      where firstLine = [
                r8&flam, rest8, rest8, triplet (r16&flam <-> l16 <-> r16 & flam)
              , r8&flam,  rest8, rest16&dot, r16&cut.drag<->singles 4 r32
              , r16&flam <-> ldrag <-> dotCut (l16<->l16&accent), singles 4 r32 & accentN 3 <-> singles 4 r32
              , r8&roll.accent <-> l8&endRoll.roll.accent, r8&endRoll.accent <-> dotCut (r16<-> l16)
              ]
            second2 = twice [para r32&zapN 0 flam<->triplet (l16<->r16<->l16&flam), singles 4 r32 & accentN 3 <-> singles 4 r32 & accentN 3 ]
            return2 = twice [singles 6 r32 & accentFirst <-> r16&drag, singles 4 r32&accentFirst <-> singles 4 r32 & accentFirst]

    ending2 = [
      dotCut (h2h r16 [flam, id, flam, flam]), singles 4 r32 <-> dotCut (r16&flam<-> l16)
      ,r32&flam<->l32<->r16<->dotCut (l16<->r16&roll), r8&endRoll, rest8
      ]

    -- dot = P.dot
    -- cut = P.cut
    dotCut = zap (cycle [dot, cut])
    dot = id
    cut = id
    accentFirst = accentN 0
    twice v = v <> v
    accentN n = elementOf _NoteHead n %~ accent

ldrag = triplet (l32 <-> r32 <-> r32)
