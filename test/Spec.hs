{-# LANGUAGE OverloadedLists #-}
import           Test.Tasty
import           Test.Tasty.Golden

import           Data.Sequence        as S
import           Score
import           Score.Library.BBCOCA
import           Score.Prelude
import           System.FilePath

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "all the things"
            [testGroup "Full Rendering"
                       [testScore "Pipe Major Donald Maclean of Lewis"
                                  "pipe-major-donald-maclean-of-lewis.png"
                                  pmDonaldMacleanOfLewis]
            ,testGroup "Signature setMomentAndStructure"
                       [
                         testScores "moment and structure"
                                  "moment-and-structure.png"
                                  [
                                  (singleParted
                                     "2/4"
                                     (Signature 2 4)
                                     (bars [singles 4 r16,singles 8 r32]))
                                  ,(singleParted
                                     "6/8 Jig"
                                     (Signature 6 8)
                                     (bars [singles 4 r16 <-> r8, l8 <-> singles 4 r16, singles 6 r16, r8<->l8<->r8]))
                                  ]
                         -- testScore "6/8 Jig"
                         --          "moment-and-structure-68jig.png"
                       ]]


testScore name expected score =
  goldenVsFile name fullExpected outputFile (writeScore Portrait PNG shortOutput score)
  where outputFile = "tmp" </> takeFileName fullExpected
        shortOutput = dropExtension outputFile
        fullExpected = ("test/expected" </> expected)

testScores name expected scores =
  goldenVsFile name fullExpected outputFile (writeScorePage Portrait PNG shortOutput scores)
  where outputFile = "tmp" </> takeFileName fullExpected
        shortOutput = dropExtension outputFile
        fullExpected = ("test/expected" </> expected)

singleParted name sig part =
  Score (Details name "" "" Nothing) sig (S.fromList [buildPart part])
