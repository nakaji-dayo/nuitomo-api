{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.System where

import           App
import           Auth
import           Data.ByteString.Char8 (pack)
import           Service.Git
import           Service.Owner

getVersionR :: AppM String
getVersionR = pure getVersion

getTwitterRedirectR :: Maybe [Char] -> Maybe String -> Maybe String -> AppM b
getTwitterRedirectR (Just lt) (Just oat) (Just oav) = do
  throwError $ err301 { errHeaders = [("Location", pack lt <> qs)] }
  where qs = "?oauth_token=" <> pack  oat <> "&oauth_verifier=" <> pack oav
getTwitterRedirectR _ _ _ = throwError err400

postPushTokensR (AccountId a) body = do
  setOwnerPushToken a body
  pure ()
