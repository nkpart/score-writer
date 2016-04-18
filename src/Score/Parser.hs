{-# language FlexibleContexts #-}
module Score.Parser where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Foldable
import Data.Functor
import Data.Monoid
import qualified Score.Prelude as P
import Control.Lens
import Score.Types
import Data.Ratio
import Text.Trifecta as T

-- | The Parsers!
---------------------------

-- | A parser of many beams. The default duration for a note is a quarter.
defaultParseBeams :: Parser [Beamed]
defaultParseBeams = evalStateT parseBeams initDuration
 where initDuration = 4

parseBeams :: StateT Integer Parser [Beamed]
parseBeams =
  whiteSpace *> sepBy parseBeamed (symbol ", ")

parseBeamed :: (MonadState Integer f, TokenParsing f) => f Beamed
parseBeamed =
   fold . fold <$> many (duration <|> note <|> triplet)

duration :: (MonadState Integer f, TokenParsing f) => f [t]
duration =
  (put =<< natural) $> []

-- | Rf~
note :: (MonadState Integer m, TokenParsing m) => m [Beamed]
note = fmap pure $ token $
           do h <- noteHand
              mods <- (appEndo . foldMap Endo) <$> many noteMod
              thisDuration <- get
              pure . mods . P.beam $ P.aNote h (1%thisDuration)

-- | { beam }
triplet :: (MonadState Integer f, TokenParsing f) => f [Beamed]
triplet = pure . P.triplet <$> braces parseBeamed

noteHand :: CharParsing f => f Hand
noteHand =
  on 'R' R <|>
  on 'L' L

noteMod :: (P.AsNoteHead (->) Identity t,P.AsDuration (->) Identity t,CharParsing f)
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
--------------------

startUnison :: TokenParsing f => f [Beamed]
startUnison =
  symbol "u(" $>
  [P.startUnison]

endUnison :: TokenParsing f => f [Beamed]
endUnison =
  symbol ")u" $>
  [P.stopUnison]

-- | Support
----------------

on :: CharParsing f => Char -> b -> f b
on ch f = char ch $> f

runBeamedParser :: String -> Either String [Beamed]
runBeamedParser input =
  let v = parseString defaultParseBeams mempty input
   in case v of
        Success e -> Right e
        Failure d -> Left (show d)
