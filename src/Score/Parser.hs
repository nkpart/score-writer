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


-- | Things that can change as we do the parsing
--    * note duration is set optionally before each note head, it carries forward
--    * Signature should be able to be changed as we go, used to do bar checks
--    * Current set of note mods written in dynamics lines above note lines
data ParseState = ParseState {
  _parseStateNoteDuration :: Integer,
  _parseStateSignature :: Signature,
  _parseStateNoteMods :: X
  }

type Mod = Endo Beamed

type X = M.Map Int64 Mod

makeLenses ''ParseState

-- | The Parsers!
---------------------------

defaultParseBeam :: Parser Beamed
defaultParseBeam = evalStateT parseBeamed initParseState

-- | A parser of many beams. The default duration for a note is a quarter.
defaultParseBeams :: Parser [Bar]
defaultParseBeams = evalStateT parseBars initParseState

-- | A parser of part - optional upbeat, beams, and repeat instructions
defaultParsePart :: Parser Part
defaultParsePart = evalStateT parsePart initParseState

defaultParseScore :: Parser Score
defaultParseScore = evalStateT (parseScore defaultSignature) initParseState
 where defaultSignature = _parseStateSignature initParseState

initParseState :: ParseState
initParseState = ParseState 4 (Signature 4 4) mempty

type MonadParseState = MonadState ParseState

-- | Syntax bits
-----------------------

parseScore :: (DeltaParsing f, MonadParseState f, TokenParsing f, MonadPlus f) => Signature -> f Score
parseScore defaultSignature =
 do (signature, details) <- symbol "details" *> T.braces (parseHeader defaultSignature)
    parseStateSignature .= signature
    theParts <- some (symbol "part" *> T.braces parsePart)
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
    do
       -- regular bars (overlaps firsttime/secondtime markers)
       bars <- barsOfLines

       -- first {  ... } second { .. }
       -- repeat
       -- <empty>

       -- First time
       rep <- let firstTimeMarker = ":1"
                  secondTimeMarker = ":2"
                  repeatSymbol = ":|"
                  ft = try (symbol firstTimeMarker) *> barsOfLines
                  st = try (symbol secondTimeMarker) *> barsOfLines
               in optional $
                  (Return <$> ft <*> st) <|>
                  (flip Return [] <$> ft) <|>
                  (Return [] <$> st) <|>
                  (try (symbol repeatSymbol) $> Repeat)
       pure $! Part bars (fromMaybe NoRepeat rep)

anacrusis :: (MonadState ParseState m, DeltaParsing m) => m (Maybe Beamed)
anacrusis =
     let anacrusisDelimeter = symbol "/"
         anacrusisLine = tokenLine parseBeamed <* anacrusisDelimeter
      in optional (try anacrusisLine)

barsOfLines :: (MonadState ParseState f, DeltaParsing f) => f [Bar]
barsOfLines =
  foldSome lineOfBars

lineOfBars :: (MonadState ParseState m, DeltaParsing m) => m [Bar]
lineOfBars =
  do optionalDynamics
     parseBars
     -- At this point, the mod state should be empty

optionalDynamics :: (MonadState ParseState f, DeltaParsing f) => f ()
optionalDynamics =
  do v <- fromMaybe M.empty <$> optional dynamicsLine
     parseStateNoteMods .= v

dynamicsLine :: DeltaParsing p => p (M.Map Int64 Mod)
dynamicsLine = token $ runUnlined (symbol "*" *> (fold <$> many dynamics))

dynamics :: DeltaParsing p => p (M.Map Int64 Mod)
dynamics = token
         $ do someSpace
              c <- column <$> position
              m <- fold <$> some noteMod
              pure $! M.singleton c m

type BarCheck =
  Bar

parseBars :: (DeltaParsing f, MonadParseState f, TokenParsing f) => f [BarCheck]
parseBars =
  -- we want to treat newlines as the end of a set of beams
  -- so we run unUnlined
  -- but after that we want to consume the newline, so we token up
  (token . T.runUnlined) (
    do v <- (fmap PartialBar <$> anacrusis)
       let go =
             do beams <- Bar Nothing <$> sepBy1 parseBeamed (symbol ",")
                beam2 <- (symbol "|" *> go) <|> (eofOrLine $> [])
                pure (beams : beam2)
       rest <- (T.newline $> []) <|> go -- must check for eof first otherwise it won't go
       pure $ maybeToList v <> rest
   )

-- _mkCheck :: Spanned Bar -> BarCheck
-- _mkCheck = undefined

eofOrLine :: CharParsing f => f ()
eofOrLine = T.newline $> () <|> T.eof

barCheck :: MonadParseState f => Bar -> f ()
barCheck bs =
  do let Sum n = bs ^. _Duration . to Sum
     s <- use parseStateSignature
     unless (n == signatureDuration s) (
       fail ("Bar duration doesn't line up with the signature:" ++ show n)
       )

parseBeamed :: (DeltaParsing f, MonadParseState f, TokenParsing f) => f Beamed
parseBeamed =
  foldSome (optional someSpace *> (note <|> triplet <|> startUnison <|> endUnison))

duration :: (MonadParseState f, TokenParsing f) => f ()
duration =
  assign parseStateNoteDuration =<< natural <?> "duration"

-- | Rf~
note :: (DeltaParsing m, MonadParseState m, TokenParsing m) => m Beamed
note = token $
       do -- Need to grab the position before we parse anything
          c <- column <$> position
          skipOptional duration
          thisDuration <- use parseStateNoteDuration
          modsForHere <- modsAtPosition c
          let noteheadP =
                do h <- noteHand
                   mods <- fold <$> many noteMod
                   let Endo mods' = modsForHere <> mods
                   pure . mods' . P.beam $ P.aNote h (1%thisDuration)
              restP =
                   on '_' (P.beam $ P.aRest (1%thisDuration))
          noteheadP <|> restP

modsAtPosition :: MonadState ParseState m
               => Int64 -> m (Endo Beamed)
modsAtPosition c =
  do x <- use parseStateNoteMods
     pure $ lookupMods x c

lookupMods :: X -> Int64 -> Endo Beamed
lookupMods x c =
  let moreMods = M.lookup c x
  in fromMaybe mempty moreMods

-- | { beam }
triplet :: (DeltaParsing f, MonadParseState f, TokenParsing f) => f Beamed
triplet = P.triplet <$> T.braces parseBeamed <?> "triplet"

noteHand :: CharParsing f => f Hand
noteHand =
  (on 'R' R <|> on 'L' L) <?> "hand"

noteMod :: CharParsing f
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

