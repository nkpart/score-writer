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
       ,testGroup "Bars" (fmap testBar barExamples)
       ,testGroup "Parts" (fmap testPart partExamples)
       ,testGroup "Scores" (fmap testScore scoreExamples)]
            where testBar = testParser P.runBarParser
                  testBeam = testParser P.runBeamParser
                  testPart = testParser P.runPartParser
                  testScore = testParser P.runScoreParser

                  beamExamples :: [(String, Beamed)]
                  beamExamples =
                    [("16R.", dot r16)
                    ,("16R. 8L-", dot r16 <> cut l8)
                    ,("16R L", r16 <> l16)
                    ,("R L", r4 <> l4)
                    ,("u( L )u", startUnison <> l4 <> stopUnison)
                    ,("{8L R L}", triplet (l8 <-> r8 <-> l8))
                    ]

                  barExamples :: [(String, [Beamed])]
                  barExamples =
                    -- These tests are subject to the bar checker, default 4/4
                    [("R, L , R,L", [r4, l4, r4, l4])
                    ,("R, L, R, L | L, R,L,R", [r4, l4, r4, l4, l4, r4, l4, r4])
                    ,("{4L R L R L R}", [ triplet (l4 <-> r4 <-> l4 <-> r4 <-> l4 <-> r4)])
                    ]

                  partExamples =
                    let rlrl = bars [r4, l4, r4, l4]
                    in
                    [("R, L, R, L", buildPart rlrl)
                    ,("R, L, R, L\nR, L, R, L\nR, L, R, L", buildPart (rlrl >> rlrl >> rlrl))
                    ,("L /\nR, L, R, L", buildPart (upbeat l4 >> rlrl))
                    ,("R, L, R, L\n:1\nR, L, R, L", buildPart (rlrl >> firstTime [r4, l4, r4, l4]))
                    ,("R, L, R, L\n:2\nR, L, R, L", buildPart (rlrl >> secondTime [r4, l4, r4, l4]))
                    ,("R,L,R,L\n:1\nR,L,R,L\n:2\nR,L,R,L", buildPart (rlrl >> firstTime [r4,l4,r4,l4] >> secondTime [r4,l4,r4,l4]))
                    ,("R, L, R, L\n:|", buildPart (rlrl >> thenRepeat))
                    ]

                  scoreExamples :: [(String, Score)]
                  scoreExamples =
                    [
                      ("===\n1L"
                     ,Score blankDetails (Signature 4 4) [buildPart (bars [l1])])
                    , ("signature 2/4\n===\n2L"
                     ,Score blankDetails (Signature 2 4) [buildPart (bars [l2])])
                    , ("signature 6/8\n===\n4L, L, L\n---R, R, R"
                     ,Score blankDetails (Signature 6 8) [buildPart (bars [l4,l4,l4]), buildPart (bars [r4,r4,r4])])
                    , ("title \"Mrs Mac\"\nstyle \"Reel\"\ncomposer \"NP\"\nband \"OCA\"\n===\n1L\n---1R"
                     ,Score (Details "Mrs Mac" "Reel" "NP" (Just "OCA")) (Signature 4 4) [buildPart (bars [l1]), buildPart (bars [r1])])
                    ]

                  testParser p (input, expected) =
                    testCase (show input) (assertEqual "" (Right expected) (p input))

renderingTests :: TestTree
renderingTests =
  testGroup "Rendering"
            [testGroup "Full Rendering"
                       [testScoreFile "score-file Pipe Major Donald Maclean of Lewis"
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
  where outputFile = "tmp" </> name <> ".png"
        shortOutput = dropExtension outputFile
        fullExpected = "test/expected" </> expected

testScoreFile name expected file =
  goldenVsFile name
               fullExpected
               outputFile
               (render PNG Portrait file shortOutput)
  where outputFile = "tmp" </> name <> ".png"
        shortOutput = dropExtension outputFile
        fullExpected = "test/expected" </> expected

singleParted name sig part =
  Score (Details name "" "" Nothing) sig [buildPart part]
