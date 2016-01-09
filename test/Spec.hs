import Test.Tasty.Golden
import Test.Tasty

import Score
import Score.Library.BBCOCA

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = goldenVsFile "Rendering test - Pipe Major Donald Maclean of Lewis"
                     "test/expected/pipe-major-donald-maclean-of-lewis.png"
                     "tmp/pipe-major-donald-maclean-of-lewis.png"
                     (writeScorePNG "tmp/pipe-major-donald-maclean-of-lewis" pmDonaldMacleanOfLewis)
