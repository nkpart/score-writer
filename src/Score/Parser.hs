{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Score.Parser where

import           Control.Monad.State.Strict
import           Data.Foldable
import Data.Monoid
import           Score.Prelude              as P
import           Text.Trifecta as T
import Data.Functor
import Data.List.NonEmpty as NE
import Control.Applicative

examples :: [(String, Beamed)]
examples =
  [("16R.", P.dot r16)
  ,("16R. 8L-", P.dot r16 <-> P.cut l8)
  ,("16R L", r16 <-> l16)
  ]

ex2 :: [(String, [Beamed])]
ex2 =
  [("16R., R.", [P.dot r16, P.dot r16])
  ]

-- |
---------------------------

noteHand :: CharParsing f => f Hand
noteHand =
  on 'R' R <|>
  on 'L' L

noteMod :: (AsNoteHead (->) Identity t,AsDuration (->) Identity t,CharParsing f)
        => f (t -> t)
noteMod =
  on '.' P.dot <|>
  on '-' P.cut <|>
  on '~' P.roll <|>
  on '^' P.accent <|>
  on 'f' P.flam <|>
  on 'd' P.drag <|>
  on 'r' P.ruff

-- | Beam tokens
---------------------------

setDuration :: (MonadState Integer f, TokenParsing f) => f [t]
setDuration = (put =<< natural) $> []

startUnison :: TokenParsing f => f [Beamed]
startUnison = symbol "u(" $> [P.startUnison]

endUnison :: TokenParsing f => f [Beamed]
endUnison = symbol ")u" $> [P.stopUnison]

note :: (MonadState Integer m, TokenParsing m) => m [Beamed]
note = fmap pure $ token $
           do h <- noteHand
              mods <- (appEndo . foldMap Endo) <$> many noteMod
              duration <- get
              pure . mods . beam $ aNote h (1%duration)

-- |
---------------------------

parseTriplet :: (MonadState Integer f, TokenParsing f) => f [Beamed]
parseTriplet = pure . triplet <$> braces parseBeamed

parseBeamed :: (MonadState Integer f, TokenParsing f) => f Beamed
parseBeamed =
   do ns <- join <$> many (setDuration <|> note <|> parseTriplet)
      return $ maybe (Beamed mempty) sconcat $ NE.nonEmpty ns

parseBeams :: StateT Integer Parser [Beamed]
parseBeams =
  whiteSpace *> sepBy parseBeamed (symbol ", ")

runBeamedParser :: String -> IO [Beamed]
runBeamedParser input =
  let p = evalStateT parseBeams 4
      v = parseString p mempty input
   in case v of
        Success e -> return e
        Failure d -> fail (show d)

on :: CharParsing f => Char -> b -> f b
on ch f = char ch $> f


main :: IO ()
main =
  do
    let parser = evalStateT parseBeamed 4
    for_ examples $ \(ex, ex') -> 
      let v = parseString parser mempty ex ^? _Success
       in print (v == pure ex', v)
    let parserM = evalStateT parseBeams 4
    for_ ex2 $ \(ex, ex') -> 
      let v = parseString parserM mempty ex ^? _Success
       in do print (v == pure ex')
             print ex'
             print v

-- |
