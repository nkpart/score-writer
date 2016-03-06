{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE TypeFamilies              #-}
module BBCOCA where

import           Score.Prelude
import           Control.Monad

msr2016 :: [[Score]]
msr2016 = [[_79ths], [dorrator], [lexy]]

su = startUnison

eu = stopUnison

_79ths :: Score
_79ths =
  Score (Details "The 79th's Farewell to Gibraltar"
                 "March"
                 "J. Reid Maxwell Feb. 27 1997"
                 Nothing)
        (Signature 2 4)
        [p1, p2, p3, p4] -- [p1, p2, p3, p4]
  where p1 = buildPart $
          do 
             upbeat (dotCut (r16<->l16))
             bars [r8&dot.flam<-> l8&cut, r8&roll<->l8
                  ,su,r4&roll,r8<->eu<->l8]
             bars [su, r8&roll<->triplet (l16<->eu<->r16<->l16), su <-> r16&dot.flam<->r16&cut.roll<->r16&dot<->l16&cut.accent <-> eu
                  ,singles 4 r32<->su<->r8&accent.roll, l8<->r16&dot<->l16&cut]

             bars [r8&dot.flam<->eu<-> l8&cut, r8&roll<->l8
                  ,su,r4&roll,r8<->eu<->l8]

             bars endingPhrase
             thenRepeat

        p2 = buildPart $
          do upbeat l8
             bars [r8&roll<->dotCut(l16<->su<->r16&accent), dotCut (l16<->l16<->r16&flam<->eu<->l16)]
             bars [triplet (r16<->l16<->r16) <-> dotCut (su<->l16&flam<->l16), r8&roll <-> triplet (l16<->r16<->l16)]
             bars [r8&flam<->eu<->r16&roll.dot<->r16&cut, dotCut (l16<->su<->r16&accent<->l16<->l16)]
             bars [r8&flam<->eu<->r8&roll, su <-> l8&roll.accent<-> singles 4 r32]
             bars [r8&accent<->eu<->r16&roll.dot<->r16&cut, dotCut (l16<->l16<->r16&flam<->su<->l16&accent)]
             bars [triplet (r16<->l16<->r16) <-> dotCut (l16&flam<->l16), r8&flam <-> eu <-> l8]
             bars endingPhrase
             thenRepeat

        p3 = buildPart $
          do upbeat (dotCut (su <-> r16<->l16))
             bars [r8&roll<->triplet (l16<->r16<->l16), triplet (r16&flam<->eu<->l16<->r16) <-> triplet (l16&flam.thisUnison <-> r16 <-> l16)]
             bars [triplet (r16<->l16<->su<->r16&flam)<->dotCut(l16<->l16), r8&roll<->triplet(l16<->r16<->l16)]
             bars [dotCut (r16&flam<->eu<->l16<->r16&roll<->r16), dotCut(l16<->su<->r16&drag<->r16<->l16&accent)]
             bars [singles 4 r32 <-> r8&roll.accent, l8<->eu<->r16&dot<->l16&cut]
             bars [r8&roll <-> triplet (l16<->r16<->l16), startUnison<->r8&roll<->l8&roll.accent]
             bars [triplet (r16<->l16<->r16&accent) <-> dotCut(l16<->l16), r8&flam, eu, l8]
             bars endingPhrase
             thenRepeat

        p4 = buildPart $
          do upbeat (dotCut (r16<->l16))
             bars [r8&roll.dot<->l8&cut, r8&roll.accent<->l8]
             bars [su, r4&roll, r8<->eu<->l8]
             bars [su,r8&roll<->l16&dot<->eu<->r16&cut.accent, dotCut (l16<->l16) <-> singles 4 r32]
             bars [r8<->su<->r8&roll, l8&roll.accent<->singles 4 r32]
             bars [r8<->dotCut(r16&roll<->r16), dotCut (l16<->l16)<->r16&drag.cut<->r16&dot.accent, eu]
             bars [l16&dot<->su<->r16&cut.roll<->triplet(r16<->l16<->r16), l8&flam<->eu<->l8]
             bars endingPhrase
             thenRepeat

        endingPhrase = [ su<->r8&roll<->l16&dot<->eu<->r16&cut.roll, r16&dot<->su<->l16&cut.accent<->singles 4 r32
                         , r8&accent<->r8&roll, l8, eu, rest8
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
            do bars [r4&flam, l4&flam, r8&ruff.dot<->su<->l8&cut.flam, r8&cut<->l8&dot.roll.accent]
               end3
               thenRepeat
        p2 = buildPart $
            do bars [su, r4&accent.roll, triplet (singles 4 r16 <-> r8), eu, triplet (l8<->r8<->l8), su, r4&roll]
               end3
               thenRepeat
        end3 = do
               bars [r8&dot<->l8&cut, r8&flam.dot<->l8&flam.cut, eu, triplet (singles 4 r16 <-> r8), triplet (l8<->r8<->l8)]
               bars [su, r4&accent.roll, triplet (singles 4 r16 <-> r8), eu, triplet (l8<->r8&drag<->r8&accent), triplet (l8<->r8<->l8)]
               bars [triplet (su<->r8<->l8<->r8<->eu) & flam, triplet (l8<->r8<->su<->l8&flam<->eu), dotCut (r8<->su<->r8&accent.ruff), l4&accent, eu]

lexy :: Score
lexy = Score (Details "Lexy McAskill"
                 "Reel"
                 ""
                 (Just "BBC Old Collegians Pipe Band"))
        (Signature 4 4)
        [p1, p2]
  where
    p1 = buildPart $
      do bars [r4&flam, su, r4&roll, triplet(l8<->r8<->l8), dotCut (r8&flam<->eu<->l8)]
         bars [r4&flam, dotCut (r8&flam<->su<->l8&accent), dotCut(r8<->r8), l8&dot.flam<->eu<->l8&cut]
         bars [r4&flam, su, r4&roll, triplet(l8<->r8<->l8), dotCut (r8&flam<->l8&accent<->eu)]
         bars [singles 4 r16, dotCut (r8&accent<->l8), su, r4&roll, l4, eu]

         thenRepeat

    p2 = buildPart $
      do upbeat l16
         bars [r4&roll.accent.thisUnison, su, l4&roll.accent, r4&roll.accent, dotCut (l8<->eu<->l8)]
         bars [triplet(r8<->l8<->r8), dotCut (l8&flam<->su<->r8&accent), dotCut(l8<->l8), dotCut(r8&flam<->eu<->l8)]
         bars [r4&roll.accent.thisUnison, su, l4&roll.accent, r4&roll.accent, dotCut (l8<->l8), eu]
         bars [singles 4 r16, dotCut (r8&accent<->l8), su, r4&roll, l4, eu]

         thenRepeat



medley2016 :: [Score]
medley2016 =
  [innerGuard
   , theCurlew, boysOfTheLough
   , shoshannas
   , ladyMackenzie
   , missGirdle
   , error "pmGeorgeAllan"
  ]

innerGuard :: Score
innerGuard =
    Score (Details "Inner Guard"
                   "4/4 March"
                   ""
                   (Just "BBC OCA Pipe Band"))
          (Signature 4 4)
          [p1, p2]
    where
      p1 = buildPart $
        do bars [su, r4&roll,r8<->eu<->singles 4 r32,r8&accent<->su<->r8&roll,l8<->eu<->triplet(r16<->l16<->r16)]
           bars [l16&flam.thisUnison<->r16<->l16<->su<->r16&flam,l16<->r16<->l8&flam,r4&flam, r4&roll]
           bars [r8&dot<->eu<->l8&cut,r8&roll<->l8,su, r8&flam<->r8&roll,triplet(l16<->r16<->l16)<->r8&flam]
           bars [l4&flam,r4&roll,r8<->r8&roll.accent,l8&roll.accent<->r8&roll.accent, eu]
           second4

      p2 = buildPart $
         do upbeat l8
            bars [su, r4&roll,r8<->eu<->singles 4 r32,r8&accent<->r8&roll,l8<->triplet(r16<->l16<->r16)]
            bars [l16&flam<->r16<->l16<->r16&flam,l16<->r16<->l8&flam,su, r8&flam<->l8&flam,r8&flam<->l8&flam]
            bars [r8&dot.flam<->eu<->l8&cut,r8&roll<->l8,su, r8&flam<->r8&roll,triplet(l16<->r16<->l16)<->r8&flam]
            bars [l4&flam,r4&roll,r8<->r8&roll.accent,l8&roll.accent<->r8&roll.accent, eu]

            second4
      second4 =
        do bars [l4,r8&flam.accent<->singles 4 r32,r8&accent<->r8&roll,l8<->triplet(r16<->l16<->r16)]
           bars [l16&flam<->r16<->l16<->r16&flam,l16<->r16<->l8&flam,r4&flam,r4&roll]
           bars [r8&roll.accent<->triplet(l16<->r16<->l16)
                ,r8&roll.accent<->triplet(l16<->r16<->l16)
                ,r8&accent.roll<->l8&roll.accent,triplet(r16&accent<->l16<->r16)<->l8&flam]
           bars [r4&flam,r4&roll,r4,rest4]

theCurlew :: Score
theCurlew = Score (Details "The Curlew"
                   ""
                   ""
                   Nothing)
          (Signature 6 8)
          [p1, p2]
    where
      p1 = buildPart $
        do bars [r4&flam.dot.accent, l4&flam, r8&flam.accent]
           bars [l8<->r8&drag<->r8, su, l8&flam<->r8<->l8&flam]
           bars [r4&flam.dot.accent, r4&roll,l8, eu]
           bars [singles 4 r16<->r8&accent, l8<->r8<->l8]

           bars [su,r4&roll.dot.accent, r4, eu, r8&drag]
           bars [r8<->l8<->r8&flam, l8&flam<->r8<->l8]
           bars [su, r8&flam<->l8<->r8&accent, eu, l8<->r8&flam<->l8&flam.accent.thisUnison]
           bars [singles 4 r16<->su<->r8&accent, l8<->r8<->l8, eu]

           thenRepeat

      p2 = buildPart $
        do bars [su, r4&roll.dot.accent, r8<->eu<->l8<->r8&drag]
           bars [singles 4 r16<->r8&accent, l8<->r8<->l8]
           bars [su, r8&flam<->l8&flam<->r8&flam, eu, l8<->r8<->l8]
           bars [r4&flam.accent.thisUnison, l8&ruff.accent,su, r8<->l8<->r8, eu]

           bars [l4&roll.dot.accent.thisUnison, su, r4&roll.accent, l8&accent.roll]
           bars [l8&roll<->r8&accent<->l8&roll.accent, l8&accent<->eu<->r8<->l8]
           bars [su, r8&flam<->l8<->r8&accent, eu, l8<->r8&flam<->l8&flam.accent.thisUnison]
           bars [singles 4 r16<->r8&accent, su, l8<->r8<->l8, eu]
           thenRepeat

boysOfTheLough :: Score
boysOfTheLough = Score (Details "Boys of the Lough"
                        ""
                        ""
                        Nothing)
                  (Signature 6 8)
                  [p1, p2]
  where
      p1 = buildPart $
        do bars [su, r4&flam.accent, l8&flam.accent, eu, singles 4 r16 <-> r8&accent] -- <->l8<->r8&drag]
           bars [l8<->r8&roll<->r8, su, r8&roll<->r8<->l8]
           bars [r8&flam.accent<->eu<->l8<->r8&flam.accent.thisUnison, l8<->r8<->su<->l8&accent]
           bars [r8<->l8&flam<->r8, l8&flam <-> singles 4 r16, eu]

           bars $
             [su, r4&accent, r8&flam.accent, eu, l8<->r8<->su<->l8&accent] <>
             [r8<->l8<->r8&flam, l8&flam<->eu<->r8&roll<->r8] <>
             [l8&accent.thisUnison<->r8<->l8, su, r16&flam<->l16<->r8<->l8&accent] <>
             [r8<->l8<->r8&flam, l4&flam.accent.dot, eu]
           thenRepeat

      p2 = buildPart $
        do upbeat l8
           bars [su, r4&flam.accent, l8&accent, r4&accent.roll, l8&accent.roll, eu]
           bars [l8&roll<->r8&accent<->l8&accent.roll, su, l8&accent<->r8<->l8]
           bars [r8&flam.accent<->eu<->l8<->r8&flam.accent.thisUnison, l8<->r8<->su<->l8&accent]
           bars [r8<->l8&flam<->r8, l8&flam <-> singles 4 r16]

           firstTime $
              [r4&accent, r8&flam.accent, eu, l8<->r8<->l8] <>
              [su, r8&roll.accent<->r8<->l8&roll.accent, l8<->r8&roll.accent<->r8] <>
              [l8&accent<->eu<->r8<->l8, su, r16&flam<->l16<->r8<->l8&accent] <>
              [r8<->l8<->r8&flam, l4&flam.accent.dot, eu]
           secondTime return'
      return' =
            [r4&flam.accent, r8&flam.accent, l8<->r8<->l8&accent] <>
            [r8<->l8<->r8&flam, l8&flam<->r8&roll<->r8] <>
            [l8&accent<->r8<->l8, r16&flam<->l16<->r8<->l8&accent] <>
            [r8<->l8<->r8&flam, l4&flam.accent.dot]

shoshannas :: Score
shoshannas =
   Score (Details "Shoshanna's Lullaby"
                  "Slow Air"
                  "BBC OCA Pipe Band"
         Nothing)
         (Signature 3 4)
         [p1]
   where
         p1 = buildPart $
           do replicateM_ 7 $ bars [rest2&dot]
              bars [rest4, rest4, r8&roll.accent<->l8&roll.accent]

              bars [r4&accent, rest8, l8, r8&roll<->l8]
              bars [su, r4&roll, r4, rest8, r8&roll]
              bars [l16&accent<->r16<->l16<->r16&flam, eu,l16<->r16&flam.thisUnison<->l16<->r16, su, l8&accent.roll<->r8&accent.roll]
              bars [l4&accent, eu, rest4, su, r4&roll]

              bars [r8<->triplet(r16<->l16<->r16), l8&flam<->l8, r8&roll<->l8]
              bars [r8&flam<->triplet(r16<->l16<->r16), l8&flam<->l8, r8&roll<->l8]
              bars [r16&flam<->l16<->r16<->l16&flam,r16<->l16&flam<->r16<->l16, r8&accent.roll<->l8&accent.roll]
              bars [r4, eu, rest4, rest4]

ladyMackenzie :: Score
ladyMackenzie =
   Score (Details "Lady MacKenzie of Fairbairn"
                  "Strathspey"
                  ""
         (Just "BBC OCA Pipe Band"))
         (Signature 4 4)
         [p1, p2]
   where ar = accent . roll
         p1 = buildPart $
         -- TODO, the second 4 bars is the same as the first, no unisons.
         -- need a function that strips them out
           do bars [r4&roll, r4, r8&ruff.dot, l8&cut, su, r4&roll]
              bars [r4, eu, triplet(singles 4 r16<-> r8&flam.thisUnison), triplet (l8<->r8<->su<->l8&flam), (r8&flam.dot<->l8&cut)]
              bars [triplet(r8<->l8<->r8&accent), r8&accent.roll.dot<->l8&cut.roll, eu, (triplet (r8&roll<->r8&accent<->l8&accent.roll)), triplet (l8&accent<->r8<->l8)]
              bars [su,r4&flam, r8&ruff.dot<->r8&drag.cut, eu, triplet(singles 4 r16<->su<->r8&accent), triplet (l8&flam<->r8<->l8), eu]

              bars [r4&roll, r4, r8&ruff.dot, l8&cut, r4&roll]
              bars [r4, triplet(singles 4 r16<-> r8&flam), triplet (l8<->r8<->l8&flam), (r8&flam.dot<->l8&cut)]
              bars [triplet(r8<->l8<->r8&accent), r8&accent.roll.dot<->l8&cut.roll, (triplet (r8&roll<->r8&accent<->l8&accent.roll)), triplet (l8&accent<->r8<->l8)]
              bars [r4&flam, r8&ruff.dot<->r8&drag.cut, triplet(singles 4 r16<->r8&accent), triplet (l8&flam<->r8<->l8), eu]

         p2 = buildPart $
           do bars $
                   [r4&ar.thisUnison, dotCut(su<->r8&ar<->l8&ar<->eu), triplet(r8&roll<->r8&accent<->l8&ar), dotCut(su<->l8&ar<->r8&ar<->eu)] <>
                   [triplet(r8&roll<->l8&accent<->r8&ar), dotCut(su<->r8&ar<->l8&ar<->eu), triplet(r8&roll<->r8&accent<->l8&ar), triplet(su<->l8&accent<->r8<->l8)] <>
                   [r4&flam, eu, triplet(singles 4 r16 <-> r8&flam.thisUnison), triplet(l8<->r8<->su<->l8&flam), dotCut(r8&flam<->l8)]
              bars [r4&flam, r8&ruff.dot<->r8&drag.cut, eu, triplet(singles 4 r16<->su<->r8&accent), triplet (l8&flam<->r8<->l8), eu]

              bars $ [r4&ar, dotCut(r8&ar<->l8&ar), triplet(r8&roll<->r8&accent<->l8&ar), dotCut(l8&ar<->r8&ar)] <>
                   [triplet(r8&roll<->l8&accent<->r8&ar), dotCut(r8&ar<->l8&ar), triplet(r8&roll<->r8&accent<->l8&ar), triplet(l8&accent<->r8<->l8)] <>
                   [r4&flam, triplet(singles 4 r16 <-> r8&flam), triplet(l8<->r8<->l8&flam), dotCut(r8&flam<->l8)]
              bars [r4&flam, r8&ruff.dot<->r8&drag.cut, triplet(singles 4 r16<->r8&accent), l4&roll]


missGirdle :: Score
missGirdle =
  Score (Details "Miss Girdle"
                 "Reel"
                 ""
        (Just "BBC OCA Pipe Band"))
        (Signature 2 2)
        [p1, p2]
  where f = flam
        a = accent
        fa = accent . flam
        r = roll
        ar = accent . roll
        p1 = buildPart $
          do bars [r4&ar,l8<->l8, r8&f<->su<->l8&f<->r8<->l8]
             bars [r4&ar<->eu,l8<->l8, su<->r8&f<->r8&drag<->singles 4 r16]
             bars [r4&a<->eu,r8&f<->l8, r8<->su<->l8&f<->r8<->l8]
             bars [r8&f<->eu<->r8&r, r8<->l8&f.thisUnison, r8<->su<->l8&f<->r8&f<->eu<->l8]
             thenRepeat
        p2 = buildPart $
          do bars [startUnison, para r16&zapN 0 f, l8<->l8&f, r8<->l8&f, r4&f, stopUnison]
             bars [r8&r<->r8 <-> l8<->r8&f, l8<->r8&drag <-> r8<->l8]
             bars [startUnison, para r16&zapN 0 f, l8<->l8&f, r8<->l8&f, r4&f]
             bars [r8&f<->eu<->r8&r, r8<->l8&f.thisUnison, r8<->su<->l8&f<->r8&f<->eu<->l8]

             bars [su<->para r16&zapN 0 f, l8<->l8&f, r8<->l8&f, r4&f]
             bars [r8&r<->r8 <-> l8<->r8&f, l8<->r8&drag <-> r8<->l8]
             bars [r4&f,r8&f<->l8, r8<->l8&f<->r8<->l8]
             bars [r8&f<->r8&r, r8<->l8&f, r2&r<->eu]

pmGeorgeAllan :: Score
pmGeorgeAllan =
   Score (Details "Pipe Major George Allan"
                  "Hornpipe"
                  ""
         (Just "BBC OCA Pipe Band"))
         (Signature 2 2)
         [p1, p2]
   where f = flam
         a = accent
         fa = accent . flam
         r = roll
         ar = accent . roll
         
         p1 = buildPart $ do
              let first2 = 
                   [su, r4&f, r8&f<->eu<->l8, r8&f <-> l8&fa <-> r8 <-> l8&f] <>
                   [r8&fa<->r8&r<->r8<->l8&fa, r8<->l8, su, r4&ar]
              bars first2
              bars [l4, eu, r8&ruff<->l8&a, singles 4 r16, r8&a<->l8]
              bars paraFinish

              bars first2
              bars [l4, eu, r8&ruff<->l8&a, singles 4 r16, su, r8&a<->l8]
              bars otherFinish
              thenRepeat

         flamPara n = para n & zapN 0 flam
         accPara n = para n & zapN 0 accent

         paraFinish = [su, flamPara r16 <-> l8&accent<->l16&flam <-> eu <->r16, l16<->l16<->r16&accent.thisUnison<->l16, r16<->r16<->su<->l8&accent, eu]
         otherFinish = [r16&flam<->eu<->l16<->r8<->l8&accent<->l8&flam, r8<->l8&flam<->r8<->l8&flam]
         p2 = buildPart $ do
               bars [su, r4&flam, r8&flam<->l8&flam, rest8, l8&flam <-> r8&flam, rest8]
               bars [r8&flam<->l8&flam <-> rest8 <-> l8&flam, flamPara r16, accPara l16]
               bars [r4&accent, eu, r8&ruff<->l8&accent, singles 4 r16, r8&accent<->l8]
               bars paraFinish

               firstTime $
                    [su, r4&flam, r8&flam<->l8&flam, rest8, l8&flam <-> r8&flam, rest8] <>
                    [r8&flam<->l8&flam <-> rest8 <-> l8&flam, flamPara r16, accPara l16] <>
                    [r4&accent, eu, r8&ruff<->l8&accent, singles 4 r16, su, r8&accent<->l8] <>
                    otherFinish
               secondTime $
                    [r4&flam, r8&flam<->l8&flam, rest8, l8&flam <-> r8&flam, rest8] <>
                    [r8&flam<->l8&flam <-> rest8 <-> l8&flam, flamPara r16, accPara l16] <>
                    [r2&accent.roll, r2&roll] <>
                    [r2&accent.roll, r2&roll] <>
                    [r4&accent, r8&ruff<->l8&accent, singles 4 r16, r8&accent<->l8] <>
                    [r8&flam<->l8 <-> r8<->l8, r2&roll] <>
                    [r2, rest2] <> [rest1]

pmDonaldMacleanOfLewis :: Score
pmDonaldMacleanOfLewis =
  Score (Details "Pipe Major Donald Maclean of Lewis"
                 "March"
                 "Nick Partridge"
                 (Just "BBC Old Collegians Pipe Band"))
        (Signature 6 8)
        [p1, p2, p3, p4]
     where
         p1 = buildPart $ do
           upbeat (l8 & thisUnison)
           upbeat l8
           bars (firstBeginning <> firstEnding <> firstBeginning <> secondEnding) 
           thenRepeat

         firstBeginning =
                  [startUnison <-> r8&flam.dot<-> r8&roll.cut <-> r8, l4&flam, r8&roll, stopUnison
                  ,triplet (l8 & accent <-> r8 <-> l8) <-> r8 & flam , l8 & flam.dot<-> r8 & cut <-> l8]
         p2 = buildPart $ do
           let beginning =
                 [r4 & roll. dot, r4 , r8 & roll,
                  l8 & accent <-> r8 & roll <-> r8 , l8 & flam . dot <-> r8 & cut <-> l8]
           bars (beginning <> firstEnding)
           firstTime (beginning <> secondEnding)
           secondTime pReturn

         p3 =
           let beginning =
                 [r8&flam.dot<->r8&roll.cut<->r8,triplet (l8<->r8<->l8)<->r8&flam
                 ,triplet (l8 <-> r8 <-> l8) <-> r8 & flam, l8 & flam . dot <-> r8 & cut <-> l8]
            in buildPart $ bars (beginning <> firstEnding <> beginning <> secondEnding) >> thenRepeat
         p4 =
           let beginning =
                 [r4 & flam, l8 & ruff, r4 & flam, r8 & roll
                 ,triplet (l8 & accent <-> r8 <-> l8) <-> r8 & flam, l8 & flam . dot <-> r8 & cut <-> l8]
            in buildPart $ do
             bars (beginning <> firstEnding)
             firstTime (beginning <> secondEnding)
             secondTime pReturn

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
