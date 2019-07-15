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
import           Query
import           Type

type UserMap = Map ResourceId User
type UserImagesMap = Map ResourceId [UserImage]
type PostImagesMap = Map ResourceId [PostImage]
type PostPostsMap = Map (ResourceId) [Post]
type PostLikesMap = Map ResourceId [Like]

-- loadTags :: ListLoader ResourceId TaskTag
-- loadTags = loadList includeTags

loadUserImages :: ListLoader ResourceId UserImage
loadUserImages = loadList includeUserImages

loadPostImages :: ListLoader ResourceId PostImage
loadPostImages = loadList includePostImages

loadUser :: Loader ResourceId User
loadUser = load (include Entity.user)

loadPostReplies :: ListLoader ResourceId Post
loadPostReplies  = loadList' includePostReplies

loadPostLikes :: String -> ListLoader ResourceId Like
loadPostLikes aid = loadList (includePostLikes aid)
