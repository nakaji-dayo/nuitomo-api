{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeOperators         #-}

module Service.Post where

import           App
-- import           Data.Type.Map as TM
import           Entity
import qualified Query  as Q
-- import           Service.Loader
-- import           Type

getPosts :: MonadService m => m [Post]
getPosts =
  queryM Q.selectPosts ()

-- -- TODO: 型注釈書くの現実的か?要検討
-- loadTasksRelation ::
--   MonadService m
--   => [Task]
--   -> Map v
--   -> m ([TaskTag],
--         Map (("tags" ':-> TMap ResourceId [TaskTag]) : v))
-- loadTasksRelation rs =
--   loadTags (Var :: Var "tags") ((^. #id) <$> rs)
