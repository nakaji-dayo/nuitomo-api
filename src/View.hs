{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeOperators         #-}

module View where

import           App
import           Control.Lens
import           Data.Type.Map  as TM
import           Entity
import           Service.Loader
import           Type
import           View.Base
-- import           View.Helper

renderTask :: ( IsMember "tags" TagsMap c
              ) =>
              Map c -> Task -> ViewM TaskResponse
renderTask loaded t = do
  ts <- getList (t ^. #id) (Var :: Var "tags") loaded
  vTags <- mapM renderTag ts
  return $ TaskResponse
      { id = t ^. #id
      , name = t ^. #name
      , tags = vTags
      }

renderTag :: TaskTag -> ViewM String
renderTag = pure . view #name