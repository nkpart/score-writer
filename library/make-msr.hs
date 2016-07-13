#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Filesystem.Path.CurrentOS

main = sh $
    do m <- using $ mktempfile "." "score.pdf"
       sr <- using $ mktempfile "." "score.pdf"
       shells ("score-writer render --portrait --score-file bbc-oca/msr/79ths.score --output-file " <> encode m) mempty
       shells ("score-writer render --portrait --score-file bbc-oca/msr/dorrator.score --score-file bbc-oca/msr/lexy.score --output-file " <> encode sr) mempty
       shells ("/usr/local/bin/gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=msr.pdf " <> encode m <> " " <> encode sr) mempty
