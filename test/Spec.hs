{-# LANGUAGE OverloadedLists #-}

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Golden

import           BBCOCA hiding (main)
import           Score
import qualified Score.Parser as P
import           Score.Prelude
import           System.FilePath

main :: IO ()
main =
  defaultMain (testGroup "Tests" [
                   renderingTests
                  ,parserTests
                  ])

parserTests :: TestTree
parserTests =
  testGroup "Parsing"
       [testGroup "Beams" (fmap testBeam beamExamples)
       ,testGroup "Parts" (fmap testPart partExamples)
       ,testGroup "Scores" (fmap testScore scoreExamples)]
            where testBeam = testParser P.runBeamedParser
                  testPart = testParser P.runPartParser
                  testScore = testParser P.runScoreParser

                  beamExamples :: [(String, [Beamed])]
                  beamExamples =
                    [("16R.", pure $ dot r16)
                    ,("16R. 8L-", pure $ dot r16 <> cut l8)
                    ,("16R L", pure $ r16 <> l16)
                    ,("R L", pure $ r4 <> l4)
                    ,("16R., R.", [dot r16, dot r16])
                    ,("u( L )u", [startUnison <> l4 <> stopUnison])
                    ,("{8L R L} R", [ triplet (l8 <-> r8 <-> l8) <-> r8])
                    ]

                  partExamples =
                    [("16L", buildPart (bars [l16]))
                    ,("16L\nL\nL", buildPart (bars [l16, l16, l16]))
                    ,("16L /\nR", buildPart (upbeat l16 >> bars [r16]))
                    ,("R\n:1\nL\n", buildPart (bars [r4] >> firstTime [l4]))
                    ,("R\n:2\nL", buildPart (bars [r4] >> secondTime [l4]))
                    ,("R\n:1\nL\n:2\nR", buildPart (bars [r4] >> firstTime [l4] >> secondTime [r4]))
                    ,("R\n:|", buildPart (bars [r4] >> thenRepeat))
                    ]

                  scoreExamples :: [(String, Score)]
                  scoreExamples =
                    [
                      ("===\nL"
                     ,Score blankDetails (Signature 4 4) [buildPart (bars [l4])])
                    , ("signature 2/4\n===\nL"
                     ,Score blankDetails (Signature 2 4) [buildPart (bars [l4])])
                    , ("signature 6/8\n===\nL\n---R"
                     ,Score blankDetails (Signature 6 8) [buildPart (bars [l4]), buildPart (bars [r4])])
                    , ("title \"Mrs Mac\"\nstyle \"Reel\"\ncomposer \"NP\"\nband \"OCA\"\n===\nL\n---R"
                     ,Score (Details "Mrs Mac" "Reel" "NP" (Just "OCA")) (Signature 4 4) [buildPart (bars [l4]), buildPart (bars [r4])])
                    ]

                  testParser p (input, expected) =
                    testCase (show input) (assertEqual "" (Right expected) (p input))



renderingTests :: TestTree
renderingTests =
  testGroup "Rendering"
            [testGroup "Full Rendering"
                       [testScores "Pipe Major Donald Maclean of Lewis"
                                  "pipe-major-donald-maclean-of-lewis.png"
                                  [pmDonaldMacleanOfLewis],
                        testScoreFile ".score Pipe Major Donald Maclean of Lewis"
                                      "pipe-major-donald-maclean-of-lewis.png"
                                      "library/pm-donald-maclean-of-lewis.score"
                       ]

            ,testGroup "Signature setMomentAndStructure"
                       [
                         testScores "moment and structure"
                                  "moment-and-structure.png"
                                  [singleParted
                                     "2/4"
                                     (Signature 2 4)
                                     (bars [singles 4 r16,singles 8 r32])
                                  ,singleParted
                                     "6/8 Jig"
                                     (Signature 6 8)
                                     (bars [singles 4 r16 <-> r8, l8 <-> singles 4 r16, singles 6 r16, r8<->l8<->r8])
                                  ,singleParted
                                     "Strathspey 4/4"
                                     (Signature 4 4)
                                     (bars [triplet (singles 4 r16 <-> r8), triplet ( l8 <-> singles 4 r16 ), triplet (singles 6 r16), triplet (r8<->l8<->r8)])
                                  ]
                       ]]


testScores name expected scores =
  goldenVsFile name fullExpected outputFile (writeScorePage Portrait PNG shortOutput scores)
  where outputFile = "tmp" </> takeFileName fullExpected
        shortOutput = dropExtension outputFile
        fullExpected = "test/expected" </> expected

testScoreFile name expected file =
  goldenVsFile name
               fullExpected
               outputFile
               (render PNG Portrait file outputFile)
  where outputFile = "tmp" </> takeFileName fullExpected
        fullExpected = "test/expected" </> expected

singleParted name sig part =
  Score (Details name "" "" Nothing) sig [buildPart part]
