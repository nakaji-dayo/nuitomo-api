-- todo: 再設計
{-# LANGUAGE TemplateHaskell #-}
module Logger where

import           Katip
import           Language.Haskell.TH

logM :: Severity -> Q Exp
logM s = [| $(logTM) s . logStr |]

logDebugM :: ExpQ
logDebugM = [| $(logM DebugS) |]

logErrorM :: ExpQ
logErrorM = [| $(logM ErrorS) |]

logInfoM :: ExpQ
logInfoM = [| $(logM InfoS) |]