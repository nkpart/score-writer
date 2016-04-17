module Score.Parser.QQ where

import Score.Parser
import Text.Trifecta as T
import Language.Haskell.TH.Quote

notes :: QuasiQuoter
notes = QuasiQuoter {
      quoteExp = \str -> do
        let c = T.parseString defaultParseBeams mempty str
        case c of
          Success r -> dataToExpQ (const Nothing) r
          Failure f -> fail (show f)
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }
