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
import           View.Helper

renderPost :: Post -> ViewM PostResponse
renderPost t = do
  -- ts <- getList (t ^. #id) (Var :: Var "tags") loaded
  return $ PostResponse
      { id = t ^. #id
      , body = t ^. #body
      }

renderUser :: (IsMember
                "userImages" UserImageMap c
              ) => Map c -> User -> ViewM UserResponse
renderUser c x = do
  is <- getList (x ^. #id) (Var :: Var "userImages") c
  isV <-  mapM renderUserImage is
  return $ UserResponse
    { id = x ^. #id
    , name = x ^. #name
    , imagePaths = isV
    }

renderUserImage :: UserImage -> ViewM String
renderUserImage x = return $ x ^. #path
