{-# LANGUAGE OverloadedLists #-}

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Golden

import           BBCOCA hiding (main)
import           Data.Sequence     as S
import           Score
import qualified Score.Parser as P
import           Score.Prelude
import           System.FilePath

main :: IO ()
main =
  defaultMain (testGroup "Tests" [
                  --  renderingTests
                   parserTests
                  ])

parserTests :: TestTree
parserTests =
  testGroup "Parsing"
       [testGroup "Beams" (fmap testBeam examples)
       ,testGroup "Parts" [testNoUpbeat, testMoreNoUpbeat, testUpbeat, testFirstTime, testSecondTime, testRepeat]]
            where testBeam (input, expected) =
                    testCase (show input) (assertEqual "" (Right expected) (P.runBeamedParser input))
                  testPart (input, expected) =
                    testCase (show input) (assertEqual "" (Right expected) (P.runPartParser input))

                  examples :: [(String, [Beamed])]
                  examples =
                    [("16R.", pure $ dot r16)
                    ,("16R. 8L-", pure $ dot r16 <> cut l8)
                    ,("16R L", pure $ r16 <> l16)
                    ,("R L", pure $ r4 <> l4)
                    ,("16R., R.", [dot r16, dot r16])
                    ]

                  testNoUpbeat =
                    testPart ("16L", buildPart (bars [l16]))
                  testMoreNoUpbeat =
                    testPart ("16L\nL\nL", buildPart (bars [l16, l16, l16]))
                  testUpbeat =
                    testPart ("16L /\nR", buildPart (upbeat l16 >> bars [r16]))
                  testFirstTime =
                    testPart ("R\n1|\nL\n", buildPart (bars [r4] >> firstTime [l4]))
                  testSecondTime =
                    testPart ("R\n2|\nL", buildPart (bars [r4] >> secondTime [l4]))
                  testRepeat =
                    testPart ("R\n:|", buildPart (bars [r4] >> thenRepeat))

renderingTests :: TestTree
renderingTests =
  testGroup "Rendering"
            [testGroup "Full Rendering"
                       [testScores "Pipe Major Donald Maclean of Lewis"
                                  "pipe-major-donald-maclean-of-lewis.png"
                                  [pmDonaldMacleanOfLewis]]
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

singleParted name sig part =
  Score (Details name "" "" Nothing) sig [buildPart part]
