{-# OPTIONS_GHC -fno-warn-orphans #-}
module Factories where

import Factory
import Entity
import App
import EntityId
import Type
import Data.Default.Class
import Control.Monad
import Entity.Follow
import Entity.Like
import Entity.Notification
import Entity.OwnerKey
import Entity.OwnerToken
import Entity.OwnerUser
import Entity.Post
import Entity.PostImage
import Entity.Question
import Entity.QuestionUser
import Entity.Report
import Entity.User
import Entity.UserImage
instance Factory Follow where
  defFactory _ i = def & #id .~ FollowId i
  persist = void . insertM insertFollow
instance Factory Like where
  defFactory _ i = def & #id .~ LikeId i
  persist = void . insertM insertLike
instance Factory Notification where
  defFactory _ i = def & #id .~ NotificationId i
  persist = void . insertM insertNotification
instance Factory OwnerKey where
  defFactory _ i = def & #id .~ OwnerKeyId i
  persist = void . insertM insertOwnerKey
instance Factory OwnerToken where
  defFactory _ i = def & #id .~ OwnerTokenId i
  persist = void . insertM insertOwnerToken
instance Factory OwnerUser where
  defFactory _ i = def & #id .~ OwnerUserId i
  persist = void . insertM insertOwnerUser
instance Factory Post where
  defFactory _ i = def & #id .~ PostId i
  persist = void . insertM insertPost
instance Factory PostImage where
  defFactory _ i = def & #id .~ PostImageId i
  persist = void . insertM insertPostImage
instance Factory Question where
  defFactory _ i = def & #id .~ QuestionId i
  persist = void . insertM insertQuestion
instance Factory QuestionUser where
  defFactory _ i = def & #id .~ QuestionUserId i
  persist = void . insertM insertQuestionUser
instance Factory Report where
  defFactory _ i = def & #id .~ ReportId i
  persist = void . insertM insertReport
instance Factory User where
  defFactory _ i = def & #id .~ UserId i
  persist = void . insertM insertUser
instance Factory UserImage where
  defFactory _ i = def & #id .~ UserImageId i
  persist = void . insertM insertUserImage
