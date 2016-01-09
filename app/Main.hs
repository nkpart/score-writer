module Main where

import Score
import Score.Library.BBCOCA
import System.Process

main :: IO ()
main = do
     writeScorePDF "pipe-major-donald-maclean-of-lewis" pmDonaldMacleanOfLewis
     callCommand "open -a Safari -g pipe-major-donald-maclean-of-lewis.pdf"
