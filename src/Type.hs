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

import           Control.Lens       ((^.))
import           Data.Int           (Int64)
import           Data.Time
import           Data.Time.Calendar
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


type Tag = String

data PostResponse = PostResponse
  { id   :: ResourceId
  , body :: String
  } deriving (Show, Generic, Eq)
$(deriveApiField ''PostResponse)

data UserResponse = UserResponse
  { id         :: ResourceId
  , name       :: String
  , imagePaths :: [String]
  } deriving (Show, Generic, Eq)
$(deriveApiField ''UserResponse)

data CreateUserRequest = CreateUserRequest
 { name      :: String
 , imagePath :: String
 } deriving (Show, Generic, Eq)
$(deriveApiField ''CreateUserRequest)
