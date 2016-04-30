{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# options_ghc -fno-warn-orphans #-}
module Score.Parser where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Foldable
import           Data.Functor
import           Data.Monoid
import           Data.Ratio
import qualified Score.Prelude                as P
import           Score.Types
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty hiding (empty, line,
                                                         (<$>), (<>))
import           Text.Trifecta                as T


data ParseState = ParseState {
  _parseStateNoteDuration :: Integer,
  _parseStateBarTime :: Sum (Ratio Integer),
  _parseStateSignature :: Signature
  } deriving (Eq, Show)

makeLenses ''ParseState

-- | The Parsers!
---------------------------

defaultParseBeam :: Parser Beamed
defaultParseBeam = evalStateT parseBeamed initDuration
 where initDuration = initParseState 4

-- | A parser of many beams. The default duration for a note is a quarter.
defaultParseBeams :: Parser [Beamed]
defaultParseBeams = evalStateT parseBeams initDuration
 where initDuration = initParseState 4

-- | A parser of part - optional upbeat, beams, and repeat instructions
defaultParsePart :: Parser Part
defaultParsePart = evalStateT parsePart initDuration
 where initDuration = initParseState 4

defaultParseScore :: Parser Score
defaultParseScore = evalStateT (parseScore defaultSignature) initDuration
 where initDuration = initParseState 4
       defaultSignature = Signature 4 4

initParseState :: Integer -> ParseState
initParseState initDuration = ParseState initDuration 0 (Signature 4 4)

type MonadParseState = MonadState ParseState

-- | Syntax bits
-----------------------

parseScore :: (MonadParseState f, TokenParsing f, MonadPlus f) => Signature -> f Score
parseScore defaultSignature =
 do (signature, details) <- parseHeader defaultSignature
    parseStateSignature .= signature
    let headerSeparator = '='
        partSeparator = '-'
    _ <- token (some (char headerSeparator))
    theParts <- parsePart `sepBy1` token (some (char partSeparator))
    pure $! Score details signature theParts

parseHeader :: (MonadParseState f,TokenParsing f,MonadPlus f)
            => Signature -> f (Signature,Details)
parseHeader s =
  execStateT p
             (s,blankDetails)
  where p =
          many (sigP <|> styleP <|> titleP <|> composerP <|> bandP)
        titleP =
          symbol "title" *> stringLiteral >>= assign (_2 . detailsTitle)
        styleP =
          symbol "style" *> stringLiteral >>= assign (_2 . detailsGenre)
        composerP =
          symbol "composer" *> stringLiteral >>= assign (_2 . detailsComposer)
        bandP =
          symbol "band" *> stringLiteral >>= (assign (_2 . detailsBand) . Just)
        sigP =
          do _ <- symbol "signature"
             d <- natural
             _ <- symbol "/"
             r <- natural
             _1 .= Signature d r

parsePart :: (MonadParseState f, TokenParsing f) => f Part
parsePart =
    do -- Upbeat (overlaps regular beams)
       let anacrusisDelimeter = "/"
       ana <- optional (try (tokenLine (parseBeamed <* symbol anacrusisDelimeter)))
       -- Regular beams (overlaps firsttime/secondtime markers)
       beams <- foldSome parseBeams
       -- First time
       rep <- let firstTimeMarker = ":1"
                  secondTimeMarker = ":2"
                  repeatSymbol = ":|"
                  ft = try (symbol firstTimeMarker) *> foldSome parseBeams
                  st = try (symbol secondTimeMarker) *> foldSome parseBeams
               in optional $
                  (Return <$> ft <*> st) <|>
                  (Return <$> ft <*> pure []) <|>
                  (Return <$> pure [] <*> st) <|>
                  (try (symbol repeatSymbol) $> Repeat)
       pure $! Part ana beams (maybe NoRepeat id rep)

parseBeams :: (MonadParseState f, TokenParsing f) => f [Beamed]
parseBeams =
  -- we want to treat newlines as the end of a set of beams
  -- so we run unUnlined
  -- but after that we want to consume the newline, so we token up
  token $
  T.runUnlined (
  do let go =
           do beams <-
                sepBy1 parseBeamed (symbol ",")
              beam2 <-
                (symbol "|" *> barCheck beams *> go) <|>
                ((T.newline $> () <|> T.eof) *> barCheck beams $> [])
              pure (beams ++ beam2)
     go)

barCheck :: (MonadParseState f) => [Beamed] -> f ()
barCheck bs =
  do let Sum n = bs ^. traverse . _Duration . to Sum
     s <- use parseStateSignature
     unless (n == signatureDuration s) $
       fail $ "BAD:" ++ show n

parseBeamed :: (MonadParseState f, TokenParsing f) => f Beamed
parseBeamed =
  foldSome (note <|> triplet <|> startUnison <|> endUnison)

duration :: (MonadParseState f, TokenParsing f) => f ()
duration =
  assign parseStateNoteDuration =<< natural <?> "duration"

-- | Rf~
note :: (MonadParseState m, TokenParsing m) => m Beamed
note = token $
       do skipOptional duration
          h <- noteHand
          mods <- (appEndo . foldMap Endo) <$> many noteMod
          thisDuration <- use parseStateNoteDuration
          pure . mods . P.beam $ P.aNote h (1%thisDuration)

-- | { beam }
triplet :: (MonadParseState f, TokenParsing f) => f Beamed
triplet = P.triplet <$> T.braces parseBeamed <?> "triplet"

noteHand :: CharParsing f => f Hand
noteHand =
  (on 'R' R <|> on 'L' L) <?> "hand"

noteMod :: (P.AsNoteHead (->) Identity t,P.AsDuration (->) Identity t,CharParsing f)
        => f (t -> t)
noteMod =
  let d s x = try (string s) $> P.dynamics x
   in
  on '.' P.dot <|>
  on '-' P.cut <|>
  on '~' P.roll <|>
  on '^' P.accent <|>
  on 'f' P.flam <|>
  on 'd' P.drag <|>
  on 'r' P.ruff <|>
  d "\\pppp\\" PPPP <|>
  d "\\ppp\\" PPP <|>
  d "\\pp\\" PP <|>
  d "\\p\\" P <|>
  d "\\ffff\\" FFFF <|>
  d "\\fff\\" FFF <|>
  d "\\ff\\" FF <|>
  d "\\f\\" F <|>
  d "\\mp\\" MP <|>
  d "\\mf\\" MF <|>
  d "\\sf\\" SF <|>
  d "\\sff\\" SFF <|>
  d "\\sp\\" SP <|>
  d "\\spp\\" SPP <|>
  d "\\sfz\\" SFZ <|>
  d "\\rfz\\" RFZ

startUnison :: TokenParsing f => f Beamed
startUnison =
  symbol "u(" $> P.startUnison <?> "start-unison"

endUnison :: TokenParsing f => f Beamed
endUnison =
  symbol ")u" $> P.stopUnison <?> "end-unison"

-- | Support
----------------

foldSome :: (Monoid b, Alternative f) => f b -> f b
foldSome p = fold <$> some p

tokenLine :: TokenParsing m => Unlined m a -> m a
tokenLine = token . T.runUnlined

on :: CharParsing f => Char -> b -> f b
on ch f = char ch $> f

runBarParser :: String -> Either String [Beamed]
runBarParser = runParser (defaultParseBeams <* eof)

runBeamParser :: String -> Either String Beamed
runBeamParser = runParser (defaultParseBeam <* eof)

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

