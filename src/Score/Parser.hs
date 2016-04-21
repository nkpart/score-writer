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
  (P.buildPart . sequence_) <$>
  (linesOf (
  -- Upbeat (overlaps regular beams)
  (try (parseBeamed <* char '/') <&> P.upbeat) <|>
  -- Regular beams (overlaps firsttime/secondtime markers)
  (try parseBeams <&> P.bars) <|>
  -- First time
  (string "1|" *> T.newline *> linesOf' parseBeams <&> P.firstTime . concat) <|>
  -- Second time
  (string "2|" *> T.newline *> linesOf' parseBeams <&> P.secondTime . concat) <|>
  -- Repeat
  (symbol ":|" $> P.thenRepeat)
  ))
  where linesOf p = p `sepEndBy1` T.newline
        linesOf' p = p `sepEndBy1` T.newline

parseBeams :: (MonadState Integer f, TokenParsing f) => f [Beamed]
parseBeams =
  sepBy1 parseBeamed (string ", ")

parseBeamed :: (MonadState Integer f, TokenParsing f) => f Beamed
parseBeamed =
  fold . fold <$> sepEndBy1 (note <|> triplet) (some (char ' '))

duration :: (MonadState Integer f, TokenParsing f) => f [t]
duration =
  (put =<< natural) $> []

-- | Rf~
note :: (MonadState Integer m, TokenParsing m) => m [Beamed]
note = fmap pure $
           do _ <- optional duration
              h <- noteHand
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
