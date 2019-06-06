module Handler.System where

import           App
import           Service.Git

getVersionR :: AppM String
getVersionR = pure getVersion