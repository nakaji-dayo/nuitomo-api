{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Network.Expo.Client
  ( module Network.Expo.Client
  , module Network.Expo.Type
  ) where

import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson
import qualified Data.ByteString.Char8  as B8
import           Data.Function
import           Network.Expo.Type
import           Network.HTTP.Simple

sendApiEndpoint = "POST https://exp.host/--/api/v2/push/send"

send :: SendRequest -> IO SendResponse
send b = request b

batchSend :: [SendRequest] -> IO BatchSendResponse
batchSend b = request b

request ::
  (MonadThrow m,
   MonadIO m,
   FromJSON b,
   ToJSON a) =>
  a -> m b
request b = do
  initReq <- parseRequest sendApiEndpoint
  let req = initReq
        & setRequestHeaders
        [ ("host",  "exp.host")
        , ("accept",  "application/json")
        , ("accept-encoding", "gzip, deflate")
        , ("content-type", "application/json")
        ]
        & setRequestBodyJSON b
  getResponseBody <$> httpJSON req
