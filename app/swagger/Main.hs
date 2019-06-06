{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import           Api
import           Control.Lens
import           Data.Aeson                 as A
import           Data.ByteString.Lazy.Char8 as LBS
import           Data.Swagger               as SW
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Auth.Server
import           Servant.Auth.Swagger       ()
import           Servant.Swagger
import           Servant.Swagger.UI
import           System.Environment

doc :: Swagger
doc = toSwagger (Proxy :: Proxy (API '[JWT]))
  & info.title .~ "nuitomo-api"
  & host ?~ "localhost:8080"

generate :: IO ()
generate = LBS.putStrLn $ encode doc

type API'' = SwaggerSchemaUI "swagger-ui" "swagger.json"

swaggerServer :: Server API''
swaggerServer = swaggerSchemaUIServer doc

swaggerAPI :: Proxy API''
swaggerAPI = Proxy

swaggerApp :: Application
swaggerApp = Servant.serve swaggerAPI swaggerServer

main :: IO ()
main = do
  cmd <- getArgs
  case cmd of
    ["ui"] -> run 8081 swaggerApp
    _      -> generate