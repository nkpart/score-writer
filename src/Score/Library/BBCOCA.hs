{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE TypeFamilies              #-}
module Score.Library.BBCOCA where

import           Score.Prelude

pmDonaldMacleanOfLewis :: Score
pmDonaldMacleanOfLewis =
  Score (Details "Pipe Major Donald Maclean of Lewis"
                 "March"
                 "Nick Partridge"
                 (Just "BBC Old Collegians Pipe Band"))
        (6,8)
        (Just l8) [p1, p2, p3, p4]
     where
         p1 = Part (firstBeginning <> firstEnding <> firstBeginning <> secondEnding) Repeat
         firstBeginning =
                  [r8&flam.dot <-> r8&roll.cut <-> r8&endRoll , l4&flam, r8&roll
                  ,triplet (l8 & accent.endRoll <-> r8 <-> l8) <-> r8 & flam , l8 & flam.dot <-> r8 & cut <-> l8]

         -- a = [[r8&flam.dot, r8&roll.cut, r8&endRoll], [l4&flam], [r8&roll]]

         p2 =
           let beginning =
                 [r4 & roll. dot, r4 & endRoll, r8 & roll,
                  l8 & endRoll . accent <-> r8 & roll <-> r8 & endRoll,
                  l8 & flam . dot <-> r8 & cut <-> l8]
           in Part
                   (beginning <> firstEnding)
                   (Return (beginning <> secondEnding, pReturn))
         p3 =
           let beginning =
                 [r8 & flam. dot <-> r8 & roll . cut <-> r8 & endRoll, triplet (l8 <-> r8 <-> l8) <-> r8 & flam
                 ,triplet (l8 <-> r8 <-> l8) <-> r8 & flam, l8 & flam . dot <-> r8 & cut <-> l8]
            in Part (beginning <> firstEnding <> beginning <> secondEnding) Repeat
         p4 =
           let beginning =
                 [r4 & flam, l8 & ruff, r4 & flam, r8 & roll
                 ,triplet (l8 & endRoll . accent <-> r8 <-> l8) <-> r8 & flam, l8 & flam . dot <-> r8 & cut <-> l8]
            in Part (beginning <> firstEnding) (Return (beginning <> secondEnding, pReturn))
         pReturn = firstBeginning <> secondEnding
         firstEnding =
           [r8 & roll <-> triplet (l8 & endRoll <-> r8 <-> l8)
           ,[Tuplet (2 % 3) (r8 & roll . accent <-> l8 & roll . endRoll . accent)]
           ,r16 & endRoll <-> l16 <-> r16<->l16<->r8
           ,l8 & dot <-> r8 & cut <-> l8]
         secondEnding =
           [r8 & flam . dot <-> r8 & roll . cut <-> r8 & endRoll ,
            r8 & roll . dot <-> r8 & endRoll . cut <-> l8 & accent . roll,
            r8 & endRoll . dot <-> l8 & cut <-> r8,
            l4 & flam,  l8]
