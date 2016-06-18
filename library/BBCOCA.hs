{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE TypeFamilies              #-}
module BBCOCA where

import           Control.Monad
import           Score
import           Score.Prelude

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

