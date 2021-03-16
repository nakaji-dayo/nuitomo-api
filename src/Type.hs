{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Type (
  module Type
  , module TH.Type
  , module Data.Time.Calendar
  , module Control.Lens
  -- , name -- 明示的に指定しないと、何故かexportされない。バグ?
  ) where

import           Control.Lens       (view, (&), (.~), (?~), (^.))
import           Data.Int           (Int64)
import           Data.Record.Extend
import           Data.Time
import           Data.Time.Calendar
import           EntityId
import           GHC.Generics
import           Instance           ()
import           TH.Type

type ResourceId = Int64

data NameAndId = NameAndId
  { id   :: ResourceId
  , name :: String
  } deriving (Show, Generic, Eq)
$(deriveApiField ''NameAndId)

data Gender = Male | Female
  deriving (Eq, Show, Generic, Enum, Bounded, Read)
$(deriveApiFieldSumType ''Gender 'Male)

data MeResponse = MeResponse
  {  key :: String
  } deriving (Show, Generic, Eq)
$(deriveApiField ''MeResponse)

type Tag = String

data UserResponse = UserResponse
  { id     :: UserId
  , name   :: String
  , images :: [String]
  , bio    :: String
  , robot  :: Bool
  } deriving (Show, Generic, Eq)
$(deriveApiField ''UserResponse)

data UserDetail= UserDetail
  { bio           :: String
  , nickname      :: String
  , gender        :: String
  , hometown      :: String
  , entryDate     :: String
  , favoriteThing :: String
  , dislikeThing  :: String
  , ownUser       :: Bool
  , isPrimary     :: Bool
  } deriving (Show, Generic, Eq)
$(deriveApiField ''UserDetail)

$(extendD "data DetailUserResponse = UserResponse <> UserDetail deriving (Show, Generic, Eq)")
$(deriveApiField ''DetailUserResponse)

data CreateUserRequest = CreateUserRequest
 { name  :: String
 , image :: String
 } deriving (Show, Generic, Eq)
$(deriveApiField ''CreateUserRequest)

data GetLike = GetLike
 { userId  :: UserId
 } deriving (Show, Generic, Eq)
$(deriveApiField ''GetLike)

data PostResponse = PostResponse
  { id          :: PostId
  , body        :: String
  , user        :: UserResponse
  , images      :: [String]
  , replyToId   :: Maybe PostId
  , replies     :: [PostResponse]
  , mentionToId :: Maybe UserId
  , createdAt   :: LocalTime
  , ownLikes    :: [GetLike]
  , likeCount   :: Int64
  , replyCount  :: Int64
  } deriving (Show, Generic, Eq)
$(deriveApiField ''PostResponse)

data CreatePostRequest= CreatePostRequest
  { userId  :: UserId
  , body    :: String
  , images  :: [String]
  , replyTo :: Maybe PostId
  } deriving (Show, Generic, Eq)
$(deriveApiField ''CreatePostRequest)

data UpdateUserRequest = UpdateUserRequest
  { name          :: Maybe String
  , image         :: Maybe String
  , bio           :: Maybe String
  , nickname      :: Maybe String
  , gender        :: Maybe String
  , hometown      :: Maybe String
  , entryDate     :: Maybe String
  , favoriteThing :: Maybe String
  , dislikeThing  :: Maybe String
  }
 deriving (Show, Generic, Eq)
$(deriveApiField ''UpdateUserRequest)

data CreateFollowRequest = CreateFollowRequest
  { toUserId  :: UserId
  } deriving (Show, Generic, Eq)
$(deriveApiField ''CreateFollowRequest)

data NotificationType = NotifyReply | NotifyFollow | NotifyLike | NotifyMention
  deriving (Eq, Show, Generic, Enum, Bounded, Read)
$(deriveApiFieldSumType ''NotificationType 'NotifyReply)

data GetNotification = GetNotification
  { id               :: NotificationId
  , user             :: UserResponse
  , notificationType :: NotificationType
  , refUser          :: Maybe UserResponse
  , refPost          :: Maybe PostResponse
  } deriving (Show, Generic, Eq)
$(deriveApiField ''GetNotification)


data CreateLikeRequest = CreateLikeRequest
  { userId :: UserId
  , postId :: PostId
  } deriving (Show, Generic, Eq)
$(deriveApiField ''CreateLikeRequest)

data OwnerResponse = OwnerResponse
  { id        :: OwnerUserId
  , key       :: String
  , isPrimary :: Bool
  } deriving (Show, Generic, Eq)
$(deriveApiField ''OwnerResponse)
