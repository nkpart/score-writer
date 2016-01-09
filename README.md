Score Writer
===

Score Writer is a library for engraving pipe band drum scores in Haskell.

An example of what can be produced is in the tests:

* https://github.com/nkpart/score-writer/blob/master/test/expected/pipe-major-donald-maclean-of-lewis.png

The score is defined here:

* https://github.com/nkpart/score-writer/blob/master/src/Score/Library/BBCOCA.hs#L11

Requirements
---

* [Stack](https://github.com/commercialhaskell/stack)
* [Lilypond](http://www.lilypond.org)

Building and Testing
---

The `lilypond` executable is used to perform the engraving and needs to be on the PATH. Otherwise, it's standard Stack.

    $ stack build --test
