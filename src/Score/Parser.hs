{-# language FlexibleContexts #-}
{-# language NoMonomorphismRestriction #-}
{-# language MultiParamTypeClasses #-}
{-# options_ghc -fno-warn-orphans #-}
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
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty hiding (line, (<>), (<$>), empty)

-- | The Parsers!
---------------------------

-- | A parser of many beams. The default duration for a note is a quarter.
defaultParseBeams :: Parser [Beamed]
defaultParseBeams = evalStateT parseBeams initDuration
 where initDuration = 4

-- | A parser of part - optional upbeat, beams, and repeat instructions
defaultParsePart :: Parser Part
defaultParsePart = evalStateT parsePart initDuration
 where initDuration = 4

defaultParseScore :: Parser Score
defaultParseScore = evalStateT (parseScore defaultSignature) initDuration
 where initDuration = 4
       defaultSignature = Signature 4 4

parseScore :: (MonadState Integer f, TokenParsing f, MonadPlus f) => Signature -> f Score
parseScore defaultSignature =
 do (signature, details) <- parseHeader defaultSignature
    _ <- token (some (char '='))
    theParts <- parsePart `sepBy1` (token (some (char '-')))
    pure $! Score details signature theParts

parseHeader :: (MonadState Integer f,TokenParsing f,MonadPlus f)
            => Signature -> f (Signature,Details)
parseHeader s =
  execStateT p (s, blankDetails)
  where p = many (sigP <|> styleP <|> titleP <|> composerP <|> bandP)
        titleP = symbol "title" *> stringLiteral >>= assign (_2 . detailsTitle)
        styleP = symbol "style" *> stringLiteral >>= assign (_2 . detailsGenre)
        composerP = symbol "composer" *> stringLiteral >>= assign (_2. detailsComposer)
        bandP = symbol "band" *> stringLiteral >>= (assign (_2 . detailsBand) . Just)
        sigP =
          do _ <- symbol "signature"
             d <- natural
             _ <- symbol "/"
             r <- natural
             _1 .= Signature d r

parsePart :: (MonadState Integer f, TokenParsing f) => f Part
parsePart =
    do -- Upbeat (overlaps regular beams)
       ana <- optional (try (tokenLine (parseBeamed <* symbol "/")))
       -- Regular beams (overlaps firsttime/secondtime markers)
       beams <- foldSome parseBeams
       -- First time
       rep <- let ft = symbol ":1" *> foldSome parseBeams
                  st = symbol ":2" *> foldSome parseBeams
               in try (Return <$> ft <*> st) <|>
                  try (Return <$> ft <*> pure []) <|>
                  try (Return <$> pure [] <*> st) <|>
                  try (symbol ":|" $> Repeat) <|>
                  pure NoRepeat
       pure $! Part ana beams rep

parseBeams :: (MonadState Integer f, TokenParsing f) => f [Beamed]
parseBeams =
  -- we want to treat newlines as the end of a set of beams
  -- so we run unUnlined
  -- but after that we want to consume the newline, so we token up
  tokenLine $ sepBy1 parseBeamed (symbol ",")

parseBeamed :: (MonadState Integer f, TokenParsing f) => f Beamed
parseBeamed =
  foldSome (note <|> triplet <|> startUnison <|> endUnison)

duration :: (MonadState Integer f, TokenParsing f) => f ()
duration =
  put =<< natural

-- | Rf~
note :: (MonadState Integer m, TokenParsing m) => m Beamed
note = token $
       do skipOptional duration
          h <- noteHand
          mods <- (appEndo . foldMap Endo) <$> many noteMod
          thisDuration <- get
          pure . mods . P.beam $ P.aNote h (1%thisDuration)

-- | { beam }
triplet :: (MonadState Integer f, TokenParsing f) => f Beamed
triplet = P.triplet <$> (symbol "{" *> parseBeamed <* symbol "}" )

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

startUnison :: TokenParsing f => f Beamed
startUnison =
  symbol "u(" $> P.startUnison

endUnison :: TokenParsing f => f Beamed
endUnison =
  symbol ")u" $> P.stopUnison

-- | Support
----------------

foldSome :: (Monoid b, Alternative f) => f b -> f b
foldSome p = fold <$> some p

tokenLine :: TokenParsing m => Unlined m a -> m a
tokenLine = token . T.runUnlined

on :: CharParsing f => Char -> b -> f b
on ch f = char ch $> f

runBeamedParser :: String -> Either String [Beamed]
runBeamedParser = runParser (defaultParseBeams <* eof)

runPartParser :: String -> Either String Part
runPartParser = runParser (defaultParsePart <* eof)

runScoreParser :: String -> Either String Score
runScoreParser = runParser (defaultParseScore <* eof)

runParser :: Parser b -> String -> Either String b
runParser p input =
  let v = parseString p mempty input
   in case v of
        Success e -> Right e
        Failure d -> Left (renderX d)

renderX :: Pretty.Doc -> String
renderX xs =
  flip Pretty.displayS "" $
  Pretty.renderCompact $ xs <> Pretty.linebreak

instance MonadState Integer f => MonadState Integer (Unlined f) where
  state = fmap Unlined state
