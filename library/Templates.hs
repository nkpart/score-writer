{-# LANGUAGE OverloadedLists #-}
module Templates where

import Score.Prelude


-- | Marches

_24March :: Score
_24March =
  Score (Details "2/4 March"
                 "2/4 March"
                 "Your Name"
                 (Just "Band Name"))
        (Signature 2 4)
        [p1, p1, p1, p1]
  where
    p1 = buildPart $
      do upbeat l16
         bars [r8<->l8,r8<->l8]
         bars [r8<->l8,r8<->l8]
         bars [r8<->l8,r8<->l8]
         bars [r8<->l8,r8<->l8]

         bars [r8<->l8,r8<->l8]
         bars [r8<->l8,r8<->l8]
         bars [r8<->l8,r8<->l8]
         bars [r8<->l8,r8<->l8]
         thenRepeat

_44March :: Score
_44March =
  Score (Details "4/4 March"
                 "4/4 March"
                 "Your Name"
                 (Just "Band Name"))
        (Signature 4 4)
        [p1, p1, p1, p1]
  where
    p1 = buildPart $
      do upbeat l8
         bars [r4,l4,r4,l4]
         bars [r4,l4,r4,l4]
         bars [r4,l4,r4,l4]
         bars [r4,l4,r4,l4]

         bars [r4,l4,r4,l4]
         bars [r4,l4,r4,l4]
         bars [r4,l4,r4,l4]
         bars [r4,l4,r4,l4]
         thenRepeat

_34March :: Score
_34March =
  Score (Details "3/4 March"
                 "3/4 March"
                 "Your Name"
                 (Just "Band Name"))
        (Signature 3 4)
        [p1, p1, p1, p1]
  where
    p1 = buildPart $
      do upbeat l8
         bars [r4,l4,r4]
         bars [l4,r4,l4]
         bars [r4,l4,r4]
         bars [l4,r4,l4]

         bars [r4,l4,r4]
         bars [l4,r4,l4]
         bars [r4,l4,r4]
         bars [l4,r4,l4]
         thenRepeat


_68March :: Score
_68March =
  Score (Details "6/8 March"
                 "6/8 March"
                 "Your Name"
                 (Just "Band Name"))
        (Signature 6 8)
        [p1, p1, p1, p1]
  where
    p1 = buildPart $
      do upbeat l8
         bars [dotCut (r8<->l8) <-> r8, dotCut (l8<->r8) <-> l8]
         bars [dotCut (r8<->l8) <-> r8, dotCut (l8<->r8) <-> l8]
         bars [dotCut (r8<->l8) <-> r8, dotCut (l8<->r8) <-> l8]
         bars [dotCut (r8<->l8) <-> r8, dotCut (l8<->r8) <-> l8]

         bars [dotCut (r8<->l8) <-> r8, dotCut (l8<->r8) <-> l8]
         bars [dotCut (r8<->l8) <-> r8, dotCut (l8<->r8) <-> l8]
         bars [dotCut (r8<->l8) <-> r8, dotCut (l8<->r8) <-> l8]
         bars [dotCut (r8<->l8) <-> r8, dotCut (l8<->r8) <-> l8]
         thenRepeat

-- | Jigs

_68Jig :: Score
_68Jig =
  Score (Details "6/8 Jig"
                 "6/8 Jig"
                 "Your Name"
                 (Just "Band Name"))
        (Signature 6 8)
        [p1, p1, p1, p1]
  where
    p1 = buildPart $
      do upbeat l8
         bars [r8<->l8<->r8, l8<->r8<->l8]
         bars [r8<->l8<->r8, l8<->r8<->l8]
         bars [r8<->l8<->r8, l8<->r8<->l8]
         bars [r8<->l8<->r8, l8<->r8<->l8]

         bars [r8<->l8<->r8, l8<->r8<->l8]
         bars [r8<->l8<->r8, l8<->r8<->l8]
         bars [r8<->l8<->r8, l8<->r8<->l8]
         bars [r8<->l8<->r8, l8<->r8<->l8]

         thenRepeat

_98Jig :: Score
_98Jig =
  Score (Details "9/8 Jig"
                 "9/8 Jig"
                 "Your Name"
                 (Just "Band Name"))
        (Signature 9 8)
        [p1, p1, p1, p1]
  where
    p1 = buildPart $
      do upbeat l8
         bars [r8<->l8<->r8, l8<->r8<->l8, r8<->l8<->r8]
         bars [l8<->r8<->l8, r8<->l8<->r8, l8<->r8<->l8]
         bars [r8<->l8<->r8, l8<->r8<->l8, r8<->l8<->r8]
         bars [l8<->r8<->l8, r8<->l8<->r8, l8<->r8<->l8]

         bars [r8<->l8<->r8, l8<->r8<->l8, r8<->l8<->r8]
         bars [l8<->r8<->l8, r8<->l8<->r8, l8<->r8<->l8]
         bars [r8<->l8<->r8, l8<->r8<->l8, r8<->l8<->r8]
         bars [l8<->r8<->l8, r8<->l8<->r8, l8<->r8<->l8]

         thenRepeat

-- | Strathspey

_Strathspey :: Score
_Strathspey =
  Score (Details "Strathspey"
                 "Strathspey"
                 "Your Name"
                 (Just "Band Name"))
        (Signature 4 4)
        [p1, p1, p1, p1]
  where
    p1 = buildPart $
      do upbeat l8
         bars [r4&roll, (triplet (r8<->l8<->r8)), l4&roll, (triplet (l8<->r8<->l8))]
         bars [r4&roll, (triplet (r8<->l8<->r8)), l4&roll, (triplet (l8<->r8<->l8))]
         bars [r4&roll, (triplet (r8<->l8<->r8)), l4&roll, (triplet (l8<->r8<->l8))]
         bars [r4&roll, (triplet (r8<->l8<->r8)), l4&roll, (triplet (l8<->r8<->l8))]

         bars [r4&roll, (triplet (r8<->l8<->r8)), l4&roll, (triplet (l8<->r8<->l8))]
         bars [r4&roll, (triplet (r8<->l8<->r8)), l4&roll, (triplet (l8<->r8<->l8))]
         bars [r4&roll, (triplet (r8<->l8<->r8)), l4&roll, (triplet (l8<->r8<->l8))]
         bars [r4&roll, (triplet (r8<->l8<->r8)), l4&roll, (triplet (l8<->r8<->l8))]

         thenRepeat

-- | Reel


_Reel :: Score
_Reel =
  Score (Details "Reel"
                 "Reel"
                 "Your Name"
                 (Just "Band Name"))
        (Signature 2 2)
        [p1, p1, p1, p1]
  where
    p1 = buildPart $
      do upbeat l8
         bars [r4, l4, r4, l4]
         bars [r4, l4, r4, l4]
         bars [r4, l4, r4, l4]
         bars [r4, l4, r4, l4]

         bars [r4, l4, r4, l4]
         bars [r4, l4, r4, l4]
         bars [r4, l4, r4, l4]
         bars [r4, l4, r4, l4]
