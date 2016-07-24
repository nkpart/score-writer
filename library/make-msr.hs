#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Filesystem.Path.CurrentOS
import Data.Text

main = sh $
    do m <- using $ mktempfile "." "m.pdf"
       sr <- using $ mktempfile "." "sr.pdf"
       let p1 = ["bbc-oca/msr/79ths.score"]
           p2 = ["bbc-oca/msr/dorrator.score", "bbc-oca/msr/lexy.score"]
       p1f <- encode <$> using (mktempfile "." "p1.score")
       p2f <- encode <$> using (mktempfile "." "p2.score")
       shells ("cat " <> (intercalate " " p1) <> " > " <> p1f) mempty
       shells ("cat " <> "styles/landscape.score " <> (intercalate " " p2) <> " > " <> p2f) mempty

       procs "score-writer" ["render", "--score-file", p1f, "--output-file", encode m] mempty
       procs "score-writer" ["render", "--score-file", p2f, "--output-file", encode sr] mempty

       -- shells ("score-writer render --portrait --score-file bbc-oca/msr/inner-guard.score --output-file " <> encode m) mempty
       -- shells ("score-writer render --portrait --score-file bbc-oca/msr/dorrator.score --score-file bbc-oca/msr/lexy.score --output-file " <> encode sr) mempty
       shells ("/usr/local/bin/gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=msr.pdf " <> encode m <> " " <> encode sr) mempty
