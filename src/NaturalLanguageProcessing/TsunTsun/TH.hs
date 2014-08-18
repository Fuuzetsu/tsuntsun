module NaturalLanguageProcessing.TsunTsun.TH (litFile) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

literally :: String -> Q Exp
literally = return . LitE . StringL

lit :: QuasiQuoter
lit = QuasiQuoter { quoteExp = literally
                  , quotePat = \s -> error s $ "quotePat: " ++ s
                  , quoteType = \s -> error $ "quoteType: " ++ s
                  , quoteDec = \s -> error $ "quoteDec: " ++ s}

-- | Read file as-is.
litFile :: QuasiQuoter
litFile = quoteFile lit
