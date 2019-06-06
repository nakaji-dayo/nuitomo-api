{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Api where

import           App
import           Auth
import qualified Config
import           Handler.Error
import           Handler.System
import           Handler.Task
import           Katip
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Servant.Auth.Server
import           Servant.Swagger.Tags
import           Type

-- instance (HasServer (MultipartForm a b :> api) context, HasMock api context) => HasMock (MultipartForm a b :> api) context where
--   mock _ ctx _ = mock (Proxy :: Proxy api) ctx

-- instance HasSwagger api => HasSwagger (MultipartForm a b :> api) where
--   toSwagger Proxy = toSwagger $ Proxy @api

type Protected =
  Tags "Task" :> TaskAPI

type TaskAPI =
  Summary "List tasks" :> "tasks" :> Get '[JSON] [TaskResponse]

type UnProtected =
  Tags "System" :>
  ( Summary "version" :> "_version" :> Get '[JSON] String
  )

type API auths = Auth auths AuthUser :> Protected
  :<|> UnProtected

protected :: AuthResult AuthUser -> ServerT Protected AppM
protected (Authenticated au) =
  taskApi au
protected _                  = throwAll err401

taskApi :: AuthUser -> ServerT TaskAPI AppM
taskApi = getTasksR

unprotected :: ServerT UnProtected AppM
unprotected =
  systemAPI
  where
    systemAPI = getVersionR

server :: ServerT (API auths) AppM
server = protected :<|> unprotected

-- server' = protected Servant.Auth.Server.NoSuchUser :<|> unprotected

-- app :: Context -> LogEnv -> Application
app :: Context -> JWTSettings -> LogEnv -> Application
app ctx@Context{config=cfg} jwtCfg logEnv =
  let
    setting = defaultCookieSettings :. jwtCfg :. EmptyContext
    corigin = case Config.deployEnv cfg of
      Config.Development -> Nothing
      _                  -> Nothing
      -- _ -> Just (BSC.pack <$> Config.deployCorsOrigins config, False)
    cors' = cors (const $ Just simpleCorsResourcePolicy
                   { corsMethods = ["GET", "HEAD", "POST", "PUT", "DELETE", "OPTIONS"]
                   , corsRequestHeaders =   [ "Accept", "Accept-Language", "Content-Language", "Content-Type", "Authorization"]
                   , corsOrigins = corigin
                   })
    psetting = Proxy :: Proxy '[CookieSettings, JWTSettings]
    hserver = hoistServerWithContext api psetting (nt ctx (logEnv, "serve") . handleGeneralException) server :: Server (API auths)
  in
    cors' $ serveWithContext api setting hserver

api :: Proxy (API '[JWT])
api = Proxy

serve :: IO ()
serve = do
  (ctx, logEnv) <- initialize
  let jwtCfg = mkJwtCfg (Config.apiSecretKey $ config ctx)
  run (Config.apiPort (config ctx)) (app ctx jwtCfg logEnv)