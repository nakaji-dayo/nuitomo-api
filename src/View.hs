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

renderPost :: (IsMember "users" UserMap c
              , IsMember "userImages" UserImagesMap c
              , IsMember "postImages" PostImagesMap c) =>
  Map c -> Post -> ViewM PostResponse
renderPost c p = do
  u <- get (p ^. #userId) (Var :: Var "users") c
  is <- getList (p ^. #id) (Var :: Var "postImages") c
  vImages <-  mapM renderImage (is :: [PostImage])
  vUser <- renderUser c u
  return $ PostResponse
      { id = p ^. #id
      , body = p ^. #body
      , user = vUser
      , images = vImages
      }

renderUser :: (IsMember
                "userImages" UserImagesMap c
              ) => Map c -> User -> ViewM UserResponse
renderUser c x = do
  is <- getList (x ^. #id) (Var :: Var "userImages") c
  isV <-  mapM renderImage (is :: [UserImage])
  return $ UserResponse
    { id = x ^. #id
    , name = x ^. #name
    , images = isV
    }

renderImage x = return $ x ^. #url
