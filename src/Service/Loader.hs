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

type UserImageMap = Map ResourceId [UserImage]

-- loadTags :: ListLoader ResourceId TaskTag
-- loadTags = loadList includeTags

loadUserImages :: ListLoader ResourceId UserImage
loadUserImages = loadList includeUserImages
