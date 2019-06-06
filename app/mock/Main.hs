{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Main where

import           Api
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant                  hiding (Context)
import           Servant.Mock


type API' = Protected :<|> UnProtected

api' :: Proxy API'
api' = Proxy

mockApp :: Application
mockApp =
  let x :: Server API'
      x = mock api' (Proxy :: Proxy '[])
  in
    serveWithContext api' undefined x

main :: IO ()
main =
  run 8082 mockApp