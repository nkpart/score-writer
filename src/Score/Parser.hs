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
import           Data.Foldable                (fold)
import           Data.Functor
import           Data.Int
import qualified Data.List.NonEmpty           as NE
import qualified Data.Map.Strict              as M
import           Data.Maybe
import           Data.Monoid                  hiding ((<>))
import           Data.Ratio
import           Data.Semigroup
import           Data.Semigroup.Foldable
import qualified Score.Prelude                as P
import           Score.Types
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty hiding (empty, line,
                                                         (<$>), (<>))
import           Text.Trifecta                as T
import           Text.Trifecta.Delta


data ParseState = ParseState {
  _parseStateNoteDuration :: Integer,
  _parseStateSignature :: Signature
  } deriving (Eq, Show)

makeLenses ''ParseState

-- | The Parsers!
---------------------------

defaultParseBeam :: Parser Beamed
defaultParseBeam = evalStateT (parseBeamed M.empty) initParseState

-- | A parser of many beams. The default duration for a note is a quarter.
defaultParseBeams :: Parser [Bar]
defaultParseBeams = evalStateT (parseBeams M.empty) initParseState

-- | A parser of part - optional upbeat, beams, and repeat instructions
defaultParsePart :: Parser Part
defaultParsePart = evalStateT parsePart initParseState

defaultParseScore :: Parser Score
defaultParseScore = evalStateT (parseScore defaultSignature) initParseState
 where defaultSignature = _parseStateSignature initParseState

initParseState :: ParseState
initParseState = ParseState 4 (Signature 4 4)

type MonadParseState = MonadState ParseState

-- | Syntax bits
-----------------------

parseScore :: (DeltaParsing f, MonadParseState f, TokenParsing f, MonadPlus f) => Signature -> f Score
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

parsePart :: (DeltaParsing f, MonadParseState f, TokenParsing f) => f Part
parsePart =
    do -- Upbeat (overlaps regular beams)
       ana <- anacrusis
       -- Regular beams (overlaps firsttime/secondtime markers)
       beams <- beamLines
       -- First time
       rep <- let firstTimeMarker = ":1"
                  secondTimeMarker = ":2"
                  repeatSymbol = ":|"
                  ft = try (symbol firstTimeMarker) *> beamLines
                  st = try (symbol secondTimeMarker) *> beamLines
               in optional $
                  (Return <$> ft <*> st) <|>
                  (flip Return [] <$> ft) <|>
                  (Return [] <$> st) <|>
                  (try (symbol repeatSymbol) $> Repeat)
       pure $! Part (maybe [] (pure . PartialBar) ana <> beams) (fromMaybe NoRepeat rep)

anacrusis :: (MonadState ParseState m, DeltaParsing m) => m (Maybe Beamed)
anacrusis =
     let anacrusisDelimeter = symbol "/"
         anacrusisLine v = anacrusisDelimeter *> tokenLine (parseBeamed v)
      in optional (try (optionalDynamics >>= anacrusisLine))

beamLines :: (MonadState ParseState f, DeltaParsing f) => f [Bar]
beamLines =
  foldSome (optionalDynamics >>= parseBeams)

type Mod = Endo Beamed

type X = M.Map Int64 Mod

optionalDynamics :: DeltaParsing p => p (M.Map Int64 Mod)
optionalDynamics =
  fromMaybe M.empty <$> optional dynamicsLine

dynamicsLine :: DeltaParsing p => p (M.Map Int64 Mod)
dynamicsLine = token $ runUnlined (symbol "*" *> (fold <$> many dynamics))

dynamics :: DeltaParsing p => p (M.Map Int64 Mod)
dynamics = token
         $ do someSpace
              c <- column <$> position
              m <- fold <$> some noteMod
              pure $! M.singleton c m

parseBeams :: (DeltaParsing f, MonadParseState f, TokenParsing f) => X -> f [Bar]
parseBeams modMap =
  -- we want to treat newlines as the end of a set of beams
  -- so we run unUnlined
  -- but after that we want to consume the newline, so we token up
  token .
  T.runUnlined $ (
  do let go =
           do beams <- Bar <$> sepBy1 (parseBeamed modMap) (symbol ",")
              beam2 <- (symbol "|" *> barCheck beams *> go) <|>
                ((T.newline $> () <|> T.eof) *> barCheck beams $> [])
              pure (beams : beam2)
     go)

barCheck :: MonadParseState f => Bar -> f ()
barCheck bs =
  do let Sum n = bs ^. _Duration . to Sum
     s <- use parseStateSignature
     unless (n == signatureDuration s) $
       fail $ "Bar duration doesn't line up with the signature:" ++ show n

parseBeamed :: (DeltaParsing f, MonadParseState f, TokenParsing f) => X -> f Beamed
parseBeamed x =
  foldSome (optional someSpace *> (note x <|> triplet x <|> startUnison <|> endUnison))

duration :: (MonadParseState f, TokenParsing f) => f ()
duration =
  assign parseStateNoteDuration =<< natural <?> "duration"

-- | Rf~
note :: (DeltaParsing m, MonadParseState m, TokenParsing m) => X -> m Beamed
note x = token $
       do c <- column <$> position
          skipOptional duration
          thisDuration <- use parseStateNoteDuration
          let noteheadP =
                do h <- noteHand
                   mods <- fold <$> many noteMod
                   let Endo mods' = lookupMods x c <> mods
                   pure . mods' . P.beam $ P.aNote h (1%thisDuration)
              restP =
                   on '_' (P.beam $ P.aRest (1%thisDuration))
          noteheadP <|> restP

lookupMods :: X -> Int64 -> Endo Beamed
lookupMods x c =
  let moreMods = M.lookup c x
  in fromMaybe mempty moreMods

-- | { beam }
triplet :: (DeltaParsing f, MonadParseState f, TokenParsing f) => X -> f Beamed
triplet x = P.triplet <$> T.braces (parseBeamed x) <?> "triplet"

noteHand :: CharParsing f => f Hand
noteHand =
  (on 'R' R <|> on 'L' L) <?> "hand"

noteMod :: (CharParsing f)
        => f (Endo Beamed)
noteMod =
  let d s x = try (string s) $> P.dynamics x
   in
  fmap Endo $
  on '.' P.dot <|>
  on '-' P.cut <|>
  on '~' P.roll <|>
  on '^' P.accent <|>
  on 'V' P.bigAccent <|>
  on 'f' P.flam <|>
  on 'd' P.drag <|>
  on 'r' P.ruff <|>
  on 'u' P.ratamacue <|>
  on '[' (P.startUnison <>) <|>
  on ']' (<> P.stopUnison) <|>
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
  d "\\rfz\\" RFZ <|>
  try (string "\\<\\") $> P.startCrescendo <|>
  try (string "\\>\\") $> P.startDecrescendo <|>
  try (string "\\.\\") $> P.endCresc

startUnison :: TokenParsing f => f Beamed
startUnison =
  symbol "u(" $> P.startUnison <?> "start-unison"

endUnison :: TokenParsing f => f Beamed
endUnison =
  symbol ")u" $> P.stopUnison <?> "end-unison"

-- | Support
----------------

foldSome :: (P.Semigroup b, Alternative f) => f b -> f b
foldSome p = fold1 . NE.fromList <$> some p

tokenLine :: TokenParsing m => Unlined m a -> m a
tokenLine = token . T.runUnlined

on :: CharParsing f => Char -> b -> f b
on ch f = char ch $> f

runBarParser :: String -> Either String [Bar]
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

instance DeltaParsing f => DeltaParsing (Unlined f) where
  line = Unlined line
  position = Unlined position
  slicedWith f (Unlined ma) = Unlined (slicedWith f ma)

