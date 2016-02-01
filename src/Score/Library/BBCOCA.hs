{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE TypeFamilies              #-}
module Score.Library.BBCOCA where

import           Score.Prelude

msr2016 :: [[Score]]
msr2016 = [[_79ths], [dorrator], [lexy]]

missGirdle :: Score
missGirdle =
  Score (Details "Miss Girdle"
                 "Reel"
                 ""
        (Just "BBC OCA Pipe Band"))
        (Signature 2 2)
        [p1, p2, p3]
  where 
        p1 = buildPart $
          do bars [r4&roll.accent,l8<->l8, r8&flam<->l8&flam<->r8<->l8]
             bars [r4&roll.accent,l8<->l8, r8&flam<->r8&drag<->singles 4 r16]
             bars [r4&accent,r8&flam<->l8, r8<->l8&flam<->r8<->l8]
             bars [r8&flam<->r8&roll, r8<->l8&flam, r8<->l8&flam<->r8&flam<->l8]
             thenRepeat
        p2 = buildPart $
          do bars [para r16&zapN 0 flam, l8<->l8&flam, r8<->l8&flam, r4&flam]
             bars [r8&roll<->r8 <-> l8<->r8&flam, l8<->r8&drag <-> r8<->l8]
             bars [para r16&zapN 0 flam, l8<->l8&flam, r8<->l8&flam, r4&flam]
             bars [r8&flam<->r8&roll, r8<->l8&flam, r8<->l8&flam<->r8&flam<->l8]

             bars [para r16&zapN 0 flam, l8<->l8&flam, r8<->l8&flam, r4&flam]
             bars [r8&roll<->r8 <-> l8<->r8&flam, l8<->r8&drag <-> r8<->l8]
             bars [para r16&zapN 0 flam, l8<->l8&flam, r8<->l8&flam, r4&flam]
             bars [r8&flam<->r8&roll, r8<->l8&flam]
             bars [r2&roll]
        p3 = buildPart $ bars [r8]
        
        
        

_79ths :: Score
_79ths =
  Score (Details "The 79th's Farewell to Gibraltar"
                 "March"
                 "J. Reid Maxwell Feb. 27 1997"
                 Nothing)
        (Signature 2 4)
        [p1, p2, p3, p4] -- [p1, p2, p3, p4]
  where p1 = buildPart $
          do let opening = [r8&dot.flam<-> l8&cut, r8&roll<->l8
                  ,r4&roll,r8<->l8]
             upbeat (dotCut (r16<->l16))
             bars opening
             bars [r8&roll<->triplet (l16<->r16<->l16), r16&dot.flam<->r16&cut.roll<->r16&dot<->l16&cut.accent
                  ,singles 4 r32<->r8&accent.roll, l8<->r16&dot<->l16&cut]
             bars opening
             bars endingPhrase
             thenRepeat

        p2 = buildPart $
          do upbeat l8
             bars [r8&roll<->dotCut(l16<->r16&accent), dotCut (l16<->l16<->r16&flam<->l16)]
             bars [triplet (r16<->l16<->r16) <-> dotCut (l16&flam<->l16), r8&roll <-> triplet (l16<->r16<->l16)]
             bars [r8&flam<->r16&roll.dot<->r16&cut, dotCut (l16<->r16&accent<->l16<->l16)]
             bars [r8&flam<->r8&roll, l8&roll.accent<-> singles 4 r32]
             bars [r8&accent<->r16&roll.dot<->r16&cut, dotCut (l16<->l16<->r16&flam<->l16&accent)]
             bars [triplet (r16<->l16<->r16) <-> dotCut (l16&flam<->l16), r8&flam <-> l8]
             bars endingPhrase
             thenRepeat

        p3 = buildPart $
          do upbeat (dotCut (r16<->l16))
             bars [r8&roll<->triplet (l16<->r16<->l16), triplet (r16&flam<->l16<->r16) <-> triplet (l16&flam <-> r16 <-> l16)]
             bars [triplet (r16<->l16<->r16&flam)<->dotCut(l16<->l16), r8&roll<->triplet(l16<->r16<->l16)]
             bars [dotCut (r16&flam<->l16<->r16&roll<->r16), dotCut(l16<->r16&drag<->r16<->l16&accent)]
             bars [singles 4 r32 <-> r8&roll.accent, l8<->r16&dot<->l16&cut]
             bars [r8&roll <-> triplet (l16<->r16<->l16), r8&roll<->l8&roll.accent]
             bars [triplet (r16<->l16<->r16&accent) <-> dotCut(l16<->l16), r8&flam, l8]
             bars endingPhrase
             thenRepeat

        p4 = buildPart $
          do upbeat (dotCut (r16<->l16))
             bars [r8&roll.dot<->l8&cut, r8&roll.accent<->l8]
             bars [r4&roll, r8<->l8]
             bars [r8&roll<->l16&dot<->r16&cut.accent, dotCut (l16<->l16) <-> singles 4 r32]
             bars [r8<->r8&roll, l8&roll.accent<->singles 4 r32]
             bars [r8<->dotCut(r16&roll<->r16), dotCut (l16<->l16)<->r16&drag.cut<->r16&dot.accent]
             bars [l16&dot<->r16&cut.roll<->triplet(r16<->l16<->r16), l8&flam<->l8]
             bars endingPhrase
             thenRepeat

        endingPhrase = [ r8&roll<->l16&dot<->r16&cut.roll, r16&dot<->l16&cut.accent<->singles 4 r32
                         , r8&accent<->r8&roll, l8, rest8
                       ]

dorrator :: Score
dorrator =
  Score (Details "Dorrator Bridge"
                 "Strathspey"
                 ""
                 (Just "BBC Old Collegians Pipe Band"))
        (Signature 4 4)
        [p1, p2]
  where p1 = buildPart $
            do bars [r4&flam, l4&flam, r8&ruff.dot<->l8&cut.flam, r8&cut<->l8&dot.roll.accent]
               end3
               thenRepeat
        p2 = buildPart $
            do bars [r4&accent.roll, triplet (singles 4 r16 <-> r8), triplet (l8<->r8<->l8), r4&roll]
               end3
               thenRepeat
        end3 = do
               bars [r8&dot<->l8&cut, r8&flam.dot<->l8&flam.cut, triplet (singles 4 r16 <-> r8), triplet (l8<->r8<->l8)]
               bars [r4&accent.roll, triplet (singles 4 r16 <-> r8), triplet (l8<->r8&drag<->r8&accent), triplet (l8<->r8<->l8)]
               bars [triplet (r8<->l8<->r8) & flam, triplet (l8<->r8<->l8&flam), dotCut (r8<->r8&accent.ruff), l4&accent]

lexy :: Score
lexy = Score (Details "Lexy McAskill"
                 "Reel"
                 ""
                 (Just "BBC Old Collegians Pipe Band"))
        (Signature 4 4)
        [] -- [p1, p2, p3, p4]

pmDonaldMacleanOfLewis :: Score
pmDonaldMacleanOfLewis =
  Score (Details "Pipe Major Donald Maclean of Lewis"
                 "March"
                 "Nick Partridge"
                 (Just "BBC Old Collegians Pipe Band"))
        (Signature 6 8)
        [p1, p2, p3, p4]
     where
         p1 = Part (Just l8) (firstBeginning <> firstEnding <> firstBeginning <> secondEnding) Repeat
         firstBeginning =
                  [r8&flam.dot <-> r8&roll.cut <-> r8, l4&flam, r8&roll
                  ,triplet (l8 & accent <-> r8 <-> l8) <-> r8 & flam , l8 & flam.dot <-> r8 & cut <-> l8]
         p2 =
           let beginning =
                 [r4 & roll. dot, r4 , r8 & roll,
                  l8 & accent <-> r8 & roll <-> r8 ,
                  l8 & flam . dot <-> r8 & cut <-> l8]
           in Part Nothing
                   (beginning <> firstEnding)
                   (Return (beginning <> secondEnding, pReturn))
         p3 =
           let beginning =
                 [r8 & flam. dot <-> r8 & roll . cut <-> r8 , triplet (l8 <-> r8 <-> l8) <-> r8 & flam
                 ,triplet (l8 <-> r8 <-> l8) <-> r8 & flam, l8 & flam . dot <-> r8 & cut <-> l8]
            in Part Nothing (beginning <> firstEnding <> beginning <> secondEnding) Repeat
         p4 =
           let beginning =
                 [r4 & flam, l8 & ruff, r4 & flam, r8 & roll
                 ,triplet (l8 & accent <-> r8 <-> l8) <-> r8 & flam, l8 & flam . dot <-> r8 & cut <-> l8]
            in Part Nothing (beginning <> firstEnding) (Return (beginning <> secondEnding, pReturn))

         pReturn = firstBeginning <> secondEnding

         firstEnding =
           [r8 & roll <-> triplet (l8 <-> r8 <-> l8)
           ,tuplet 2 3 (r8 & roll . accent <-> l8 & roll . accent)
           ,r16 <-> l16 <-> r16<->l16<->r8
           ,l8 & dot <-> r8 & cut <-> l8]

         secondEnding =
           [r8 & flam . dot <-> r8 & roll . cut <-> r8 ,
            r8 & roll . dot <-> r8 & cut <-> l8 & accent . roll,
            r8 & dot <-> l8 & cut <-> r8,
            l4 & flam,  l8]
