{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Network.Expo.Type where

import           Data.Aeson
import           GHC.Generics

customOptions = defaultOptions
  { fieldLabelModifier = escape
  , omitNothingFields = True
  }
  where
    escape "_data" = "data"
    escape x       = x

data Priority = Default | Nnormal | High
  deriving (Generic, Show)
instance ToJSON Priority where


data SendRequest = SendRequest
  { to                   :: String
  , _data                :: Maybe Object
  , title                :: Maybe String
  , body                 :: Maybe String
  , ttl                  :: Maybe Int
  , expiration           :: Maybe Int
  , priority             :: Maybe Priority
  , sound                :: Maybe String
  , badge                :: Maybe Int
  , _displayInForeground :: Maybe Bool
  } deriving (Generic, Show)
instance ToJSON SendRequest where
  toJSON     = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

sendRequest to = SendRequest
  { to         = to
  , _data      = Nothing
  , title = Nothing
  , body = Nothing
  , ttl = Nothing
  , expiration = Nothing
  , priority = Nothing
  , sound = Nothing
  , badge = Nothing
  , _displayInForeground = Nothing
  }

data SendResult = SendResult
  { status :: String
  , id     :: String
  } deriving (Generic, Show)
instance FromJSON SendResult where
  parseJSON = genericParseJSON customOptions

data SendResponse = SendResponse
  { _data :: SendResult
  } deriving (Generic, Show)
instance FromJSON SendResponse where
  parseJSON = genericParseJSON customOptions

data BatchSendResponse = BatchSendResponse
  { _data :: [SendResult]
  } deriving (Generic, Show)
instance FromJSON BatchSendResponse where
  parseJSON = genericParseJSON customOptions
