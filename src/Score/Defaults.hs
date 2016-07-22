module Score.Defaults where

import Score.Types
import Score.Render

defaultNoteLength :: Num a => a
defaultNoteLength = 4

defaultSignature :: Signature
defaultSignature = Signature 4 4

defaultBarsPerLine :: Num a => a
defaultBarsPerLine = 4

defaultOrientation :: Orientation
defaultOrientation = Portrait
