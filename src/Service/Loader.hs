{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Service.Loader
  ( module Service.Loader
  , TMap
  ) where

import           Data.Map    (Map)
import           EagerLoader
import           Entity
import           EntityId
import           Query

type UserMap = Map UserId User
type UserImagesMap = Map UserId [UserImage]
type PostImagesMap = Map PostId [PostImage]
type PostPostsMap = Map PostId [Post]
type PostLikesMap = Map PostId [Like]

-- loadTags :: ListLoader ResourceId TaskTag
-- loadTags = loadList includeTags

loadUserImages :: ListLoader UserId UserImage
loadUserImages = loadList includeUserImages

loadPostImages :: ListLoader PostId PostImage
loadPostImages = loadList includePostImages

loadUser :: Loader UserId User
loadUser = load (include Entity.user)

loadPostReplies :: ListLoader PostId Post
loadPostReplies  = loadList' includePostReplies

loadPostLikes :: String -> ListLoader PostId Like
loadPostLikes aid = loadList (includePostLikes aid)
