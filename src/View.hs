{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeOperators         #-}

module View where

import           App
import           Control.Lens
import           Data.Default.Class
import           Data.Generics.Product.Subtype
import           Data.Type.Map                 as TM
import           Entity
import           Service.Loader
import           Type
import           View.Base
import           View.Helper

renderPost :: (IsMember "users" UserMap c
              , IsMember "userImages" UserImagesMap c
              , IsMember "postImages" PostImagesMap c
              , IsMember "replies" PostPostsMap c) =>
  Map c -> Post -> ViewM PostResponse
renderPost c p = do
  u <- get (p ^. #userId) (Var :: Var "users") c
  is <- getList (p ^. #id) (Var :: Var "postImages") c
  rs <- getList (p ^. #id) (Var :: Var "replies") c
  vImages <-  mapM renderImage (is :: [PostImage])
  vUser <- renderUser c u
  vReplies <-  mapM (renderPost c) rs
  return $ PostResponse
      { id = p ^. #id
      , body = p ^. #body
      , user = vUser
      , images = vImages
      , replies = vReplies
      , replyToId = p ^. #replyTo
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

renderDetailUser :: (IsMember
                "userImages" UserImagesMap c
              ) => Map c -> User -> ViewM DetailUserResponse
renderDetailUser c x = do
  u <- renderUser c x
  return $ smash u (def :: DetailUserResponse)
    { bio = x ^. #bio
    , nickname = x ^. #nickname
    , gender = x ^. #gender
    , hometown = x ^. #hometown
    , entryDate = x ^. #entryDate
    , favoriteThing = x ^. #favoriteThing
    , dislikeThing = x ^. #dislikeThing
    }

renderImage x = return $ x ^. #url
