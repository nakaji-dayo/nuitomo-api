{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.System where

import           App
import           Data.ByteString.Char8 (pack)
import           Data.Maybe
import           Service.Git

getVersionR :: AppM String
getVersionR = pure getVersion

getTwitterRedirectR (Just lt) (Just oat) (Just oav) = throwError $ err301 { errHeaders = [("Location", pack lt <> qs)] }
  where qs = "?oauth_token=" <> pack  oat <> "&oauth_verifier=" <> pack oav
getTwitterRedirectR _ _ _ = throwError err400
