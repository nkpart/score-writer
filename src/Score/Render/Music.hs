module Score.Render.Music where

-- | Data.Music.Lilypond utilities
import qualified Data.Music.Lilypond as L

override :: Show a => String -> a -> L.Music
override k v = L.Override k (L.toValue v)

overrideL :: String -> String -> L.Music
overrideL k v = L.Override k (L.toLiteralValue v)
