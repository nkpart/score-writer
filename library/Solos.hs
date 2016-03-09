{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE NoMonomorphismRestriction          #-}
module Solos where

import Score.Prelude hiding (dot, cut)
import qualified Score.Prelude as P (dot, cut)

jimmyBlue :: Score
jimmyBlue =
  Score (Details "Jimmy Blue"
                 "Hornpipe"
                 "Arthur Cook"
                 Nothing)
        (Signature 2 4)
        [p1, p2, p3, p4]
  where
    p1 = buildPart $
      do let commonStart = bars
              [r8 & flam<->dotCut (l16&ruff<->r16 & accent), dotCut (l16<->l16<->r16&drag<->r16&accent)
              ,dotCut (l16<->r16<->l16&drag<->l16 & accent), singles 4 r32 <-> dotCut (r16 & accent <-> l16)
              ]
         commonStart
         bars [dotCut (h2h r16 [flam,id,flam,flam]), singles 4 r32 <-> singles 4 r32 & accentFirst
              ,r8&accent.roll<->l8&accent.roll, dotCut (r16&accent.roll<->r16&accent<->l16&accent.roll<->l16&accent)]
         commonStart
         bars ending2
         thenRepeat

    p2 = buildPart $
      do let commonStart =
              let b1 = [ para r32&zapN 0 flam <-> dotCut (l16 <-> l16 & flam) , dotCut (r16 <-> r16 & flam <-> l16 <-> l16 & flam )  ]
               in b1 <> fmap swapHands b1
         bars commonStart
         bars [para r32&zapN 0 flam<->dotCut (l16<->l16&flam), singles 4 r32 <-> r8&accent.roll
              ,dotCut(l16&roll.accent<->l16&accent)<->singles 4 r32, r8&accent<->dotCut(r16&flam<->r16&flam)]
         let
           return2 = -- the start of this movement should be dot/cut in both rounded and swung
             let v = r16&P.dot.accent<->l16&P.cut.accent<->singles 4 r32&accentN 3<->singles 4 r32&accentFirst<->singles 4 r32&accentFirst
              in [v,v]
         firstTime $ commonStart <> ending2
         secondTime $ return2 <> ending2

    --- PART 3
    p3 = buildPart $ bars (first2 <> end2' <> first2 <> ending2) >> thenRepeat
      where first2 =
              [dotCut (r16&flam<->l16&roll<->l16<->r16&roll), dotCut(r16<->l16<->r16<->l16&flam)
              ] <> (let v = r8&flam.roll<->dotCut (l16<->r16) in [v, swapHands v])
            end2' =
              [dotCut (r16&flam<->l16<->r16&flam<->l16&flam), singles 4 r32 & accentN 3 <-> singles 4 r32
              ,dotCut (r16&drag<->r16&accent<->l16<->r16&drag), singles 4 r32 <-> dotCut (r16&accent<->l16)]

    --- PART 4
    p4 = buildPart $ bars firstLine >> firstTime (second2 <> ending2) >> secondTime (return2 <> ending2)
      where firstLine = [
                r8&flam, rest8, rest8, triplet (r16&flam <-> l16 <-> r16 & flam)
              , r8&flam,  rest8, rest16&dot, r16&cut.drag<->singles 4 r32
              , r8&flam <-> dotCut (l16&ruff<->l16&accent), singles 4 r32 & accentN 3 <-> singles 4 r32
              , r8&roll.accent <-> l8&roll.accent, r8&accent <-> dotCut (r16<-> l16)
              ]
            second2 = twice [para r32&zapN 0 flam<->triplet (l16<->r16<->l16&flam), singles 4 r32 & accentN 3 <-> singles 4 r32 & accentN 3 ]
            return2 = twice [singles 6 r32 & accentFirst <-> r16&drag, singles 4 r32&accentFirst <-> singles 4 r32 & accentFirst]

    ending2 = [
      dotCut (h2h r16 [flam, id, flam, flam]), singles 4 r32 <-> dotCut (r16&flam<-> l16)
      ,r32&flam<->l32<->r16<->dotCut (l16<->r16&roll), r8, rest8
      ]

    -- dot = P.dot
    -- cut = P.cut
    dot = id
    cut = id
    accentFirst = accentN 0
    twice v = v <> v
    accentN n = elementOf _NoteHead n %~ accent

ldrag = triplet (l32 <-> r32 <-> r32)

-- | Reel

mrsMac :: Score
mrsMac =
   Score (Details "Mrs MacPherson of Inveran"
                  "Reel"
                  "Dean Hall"
                  Nothing)
         (Signature 2 2)
         [p1, p2, p3, p4, p5, p6]
  where
    rlrl_16 = singles 4 r16
    lrlr_16 = singles 4 l16
    a = accent
    f = flam
    r = roll
    ar = accent . roll
    fa = flam . accent
    dc = dotCut
    t3 = triplet
    p1 = buildPart $
     do let f2 = do
                bars [fa r4, dc(l8<->a r8), dc(l8<->l8<->fa r8<->l8)]
                bars [r r4, dc(l8<->a l8), rlrl_16 <-> dc(a r8<->l8)]
        f2
        bars [fa r4, rlrl_16, dc(r8<->l8<->fa r8<->r r8)]
        bars [dc(r8<->fa r8<->l8<->fa l8), rlrl_16<->dc(a r8<->l8)]

        f2
        bars [fa r4, rlrl_16, dc(r8<->l8)<->t3(r8&a<->l8<->r8)]
        bars [t3(l8<->r8<->a l8)<->rlrl_16, ar r4, a l4]
    p2 = buildPart $
     do let f2 = do
                 bars [r4&r, dc(l8<->r8&r), dc(r8<->l8), r4&r]
                 bars [t3(l8<->r8<->l8), dc(r8&a<->l8), dc(r8&f<->l8&a) <-> rlrl_16 ]
        upbeat l16
        f2
        bars [r4&a, t3(r8&f<->l8<->r8&a), dc(l8<->l8<->r8&fa<->l8)]
        bars [r4&a.r,l4&a.r<->rlrl_16<->dc(r8&a<->l8)]

        f2
        bars [dc(r8&a<->r8&r) <-> t3(r8<->l8<->r8&a), dc(l8<->l8) ,r4&f.a]
        bars [dc (l8&ruff<->l8&a) <-> (rlrl_16 & zapN 3 a), rlrl_16&zapN 3 a, r4&a]
    p3 = buildPart $
         do upbeat l16
            bars [fa r4, t3(f r8<->l8<->a r8), dc(l8<->l8<->fa r8<->r r8)]
            bars [t3(r8<->l8<->r8)<->dc(a l8<->l8), dc(f r8<->a l8)<->rlrl_16]
            bars [r8<->zapN 0 a rlrl_16<->drag r8, rlrl_16<->dc(a r8<->l8)]
            bars [drag r8<->zapN 0 a rlrl_16<->drag r8, rlrl_16<->dc(a r8<->l8)]

            bars [fa r4, t3(f r8<->l8<->a r8), dc(l8<->l8<->fa r8<->r r8)]
            bars [t3(r8<->l8<->r8)<->dc(a l8<->l8), dc(f r8<->a l8)<->rlrl_16]
            bars [dc(r8&a<->r8&r) <-> t3(r8<->l8<->r8&a), dc(l8<->l8)<->t3(r8&f<->l8<->r8&a)]
            bars [dc (l8<->r8&f.a<->l8<->l8&f.a)<->rlrl_16, r4&a]

    p4 = buildPart $
         do upbeat l16
            bars [dc(ar r8<->r l8<->r l8<->ar l8), dc(r l8<->a r8<->ar l8<->r l8)]
            bars [dc(r l8<->ar r8<->r l8<->ar l8), dc(r l8<->a r8<->ar l8<->r l8)]
            bars [dc(ar r8<->a r8<->ar l8<->ar l8), dc(r l8<->a r8)<->t3(l8<->r8<->l8)]
            bars [dc(ar r8<->a r8<->ar l8<->ar l8), dc(r l8<->a r8)<->t3(l8<->r8<->a l8)]

            bars [dc(ar r8<->r l8<->r l8<->ar l8), dc(r l8<->a r8<->ar l8<->r l8)]
            bars [dc(r l8<->ar r8<->r l8<->ar l8), dc(r l8<->a r8<->ar l8<->r l8)]

            bars [dc(r8&a<->r8&r) <-> t3(r8<->l8<->r8&a), dc(l8<->l8) ,r4&f.a]
            bars [dc (l8&ruff<->l8&a) <-> (rlrl_16 & zapN 3 a), rlrl_16&zapN 3 a, r4&a]
    p5 = buildPart $
     do bars [dc(f r8<->a l8)<->rlrl_16, dc(r8<->a r8)<->lrlr_16]
        bars [dc(l8<->a l8)<->zapN 3 a rlrl_16, zapN 3 a rlrl_16<->dc(a r8<->l8)]
        bars [dc(fa r8<->fa l8)<->rest8&P.dot<->l8&P.cut,r r4, dc(r l8<->l8)]
        bars [ar r4, ar l4, rlrl_16 <-> dc(a r8<->l8)]

        bars [dc(f r8<->a l8)<->rlrl_16, dc(r8<->a r8)<->lrlr_16]
        bars [dc(l8<->a l8)<->zapN 3 a rlrl_16, zapN 3 a rlrl_16<->dc(a r8<->l8)]
        bars [f r4,f l4, (zapN 0 a . zapN 5 a) $ tuplet 6 4 (singles 6 r8)]
        bars [dc(r8<->fa r8<->l8<->fa l8), rlrl_16, a r4]

    p6 = buildPart $ 
     do bars $ [tuplet 6 4 (singles 6 r8), rlrl_16 <-> t3 (r8<->l8<->r8)]
          & zap [a, id, id, id, id, a,     id, id, id, id,            a]
        bars $ [t3 (singles 3 l8) <-> rlrl_16, t3 (singles 3 r8)<-> dc(l8<->l8)]
          & zap [id, id, a, id, id, id, id, id, id, a]
        bars $ [r8 <-> rlrl_16 <-> r8, rlrl_16 <-> dc (r8<->l8)]
          & zap [drag, a, id, id, id, drag, id, id, id, id, a]
        bars $ [r8 <-> rlrl_16 <-> r8, rlrl_16 <-> dc (r8<->l8)]
          & zap [drag, a, id, id, id, drag, id, id, id, id, a]

        bars $ [tuplet 6 4 (singles 6 r8), rlrl_16 <-> t3 (r8<->l8<->r8)]
          & zap [a, id, id, id, id, a,     id, id, id, id,            a]
        bars $ [t3 (singles 3 l8) <-> rlrl_16, t3 (singles 3 r8)<-> dc(l8<->l8)]
          & zap [id, id, a, id, id, id, id, id, id, a]
        bars $ [rlrl_16 <-> dc(r8<->l8), singles 8 r16]
          & zap [id,id,id,id,id, a, id,id,id, a, id, id, id, a]
        bars $ [t3 (r8<->l8<->r8)<->lrlr_16, l4, r4]
          & zap [a, id, a, id,id,id, a, ar, a]
