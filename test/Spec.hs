import           Test.HUnit ((~?))
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

-- import           BBCOCA            hiding (main)
import           LilypondProcess
import           Score
import qualified Score.Parser      as P
import           Score.Prelude
import Score.Render
import Control.Monad (when)
import           System.Directory
import           System.FilePath

main :: IO ()
main =
  defaultMain (testGroup "Tests" [
                   renderingTests
                  ,parserTests
                  ,lilypondProcessTests
                  ])

lilypondProcessTests :: TestTree
lilypondProcessTests =
  let test =
        (do runLilypond PNG "x.png" "{ c' }"
            v <- doesFileExist "x.png"
            when v (removeFile "x.png")
            pure v) @? "runLilypond writes to the requested file"
  in testCase "runLilypond" test

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
                    ,("L[]", (startUnison . stopUnison) l4)
                    ,("{8L R L}", triplet (l8 <-> r8 <-> l8))
                    ]

                  barExamples :: [(String, [Bar])]
                  barExamples =
                    -- These tests are subject to the bar checker, default 4/4
                    [("R, L , R,L", pure $ Bar [r4, l4, r4, l4])
                    ,("R, L, R, L | L, R,L,R", [Bar [r4, l4, r4, l4], Bar [l4, r4, l4, r4]])
                    ,("{4L R L R L R}", pure $ Bar [ triplet (l4 <-> r4 <-> l4 <-> r4 <-> l4 <-> r4)])
                    ]

                  partExamples =
                    let rlrl = bar [r4, l4, r4, l4]
                    in
                    [("R, L, R, L", part & rlrl)
                    ,(" R, L, R, L", part & rlrl)
                    ,("R, L, R, L\nR, L, R, L\nR, L, R, L", part & rlrl & rlrl & rlrl)
                    ,("L /\nR, L, R, L", part & upbeat l4 & rlrl)
                    ,("L / R, L, R, L", part & upbeat l4 & rlrl)
                    -- TODO: this doesn't parse, but not sure it needs to or should
                    -- ,("R, L, R, L\n:1\nR, L, R, L", buildPart (rlrl >> firstTime [r4, l4, r4, l4]))
                    ,("R, L, R, L\n:2\nR, L, R, L", part & rlrl & secondTime [Bar [r4, l4, r4, l4]])
                    ,("R,L,R,L\n:1\nR,L,R,L\n:2\nR,L,R,L", part & rlrl & firstTime [Bar [r4,l4,r4,l4]] & secondTime [Bar [r4,l4,r4,l4]])
                    ,("R, L, R, L\n:|", part & rlrl & thenRepeat)
                    -- Accent/Dynamics lines
                    ,(unlines ["* ^ "
                              ,"  R, L, R, L"],
                       part & bar [accent r4, l4, r4, l4])
                    ]

                  scoreExamples :: [(String, Score)]
                  scoreExamples =
                    [
                      -- TODO: get ride of the newlines at the end of parts
                      ("score { details { }\npart { 1L\n } }"
                     ,Score blankDetails (Signature 4 4) 4 [part & (bar [l1])])
                    , ("score { details { signature 2/4 }\npart { 2L\n } }"
                     ,Score blankDetails (Signature 2 4) 4 [part & (bar [l2])])
                    , ("score { details { signature 6/8 }\npart { 4L, L, L\n } part { R, R, R \n } }"
                     ,Score blankDetails (Signature 6 8) 4 [part & (bar [l4,l4,l4]), part & (bar [r4,r4,r4])])
                    , ("score { details { title \"Mrs Mac\"\nstyle \"Reel\"\ncomposer \"NP\"\nband \"OCA\" }\n part {1L\n } part {1R \n } }"
                     ,Score (Details "Mrs Mac" "Reel" "NP" (Just "OCA")) (Signature 4 4) 4 [part & (bar [l1]), part & (bar [r1])])
                    ]

                  testParser p (input, expected) =
                    do
                    testCase (show input) (do  let v = p input
                                               case v of
                                                 Right x -> assertEqual "" expected x
                                                 Left e -> fail e
                                               assertEqual "" (Right expected) (p input))

renderingTests :: TestTree
renderingTests =
  testGroup "Rendering"
            [testGroup "Full Rendering"
                       [testScoreFile "score-file Pipe Major Donald Maclean of Lewis"
                                      "pipe-major-donald-maclean-of-lewis.png"
                                      "library/bbc-oca/pm-donald-maclean-of-lewis.score"
                       ,testScoreFile "score-file 79ths"
                                      "79ths.png"
                                      "library/bbc-oca/msr/79ths.score"
                       ]

            ,testGroup "Signature setMomentAndStructure"
                       [
                         testScores "moment and structure"
                                  "moment-and-structure.png"
                                  [singleParted
                                     "2/4"
                                     (Signature 2 4)
                                     (bar [singles 4 r16,singles 8 r32])
                                  ,singleParted
                                     "6/8 Jig"
                                     (Signature 6 8)
                                     (bar [singles 4 r16 <-> r8, l8 <-> singles 4 r16, singles 6 r16, r8<->l8<->r8])
                                  ,singleParted
                                     "Strathspey 4/4"
                                     (Signature 4 4)
                                     (bar [triplet (singles 4 r16 <-> r8), triplet ( l8 <-> singles 4 r16 ), triplet (singles 6 r16), triplet (r8<->l8<->r8)])
                                  ]
                       ]]

testScores name expected scores =
  goldenVsFile name fullExpected outputFile (writeScorePage PNG shortOutput (opts, scores))
  where outputFile = "tmp" </> name <> ".png"
        shortOutput = outputFile
        opts = RenderingOptions Portrait
        fullExpected = "test/expected" </> expected

testScoreFile name expected file =
  goldenVsFile name
               fullExpected
               outputFile
               (render PNG file shortOutput)
  where outputFile = "tmp" </> name <> ".png"
        shortOutput = outputFile
        fullExpected = "test/expected" </> expected

singleParted name sig p =
  Score (Details name "" "" Nothing) sig 4 [part & p]
