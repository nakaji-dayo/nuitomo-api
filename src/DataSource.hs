module DataSource (
  connect, defineEntity
  , connectWithLoadConfig
  , createPool'
  , Pool
  , withResource
  , destroyAllResources
  , Connection
  ) where

import           Config
import           Data.Pool
import           Database.HDBC                   (disconnect)
import           Database.HDBC.PostgreSQL        (Connection, connectPostgreSQL)
import           Database.HDBC.Query.TH          (defineTableFromDB)
import           Database.HDBC.Schema.Driver     (typeMap)
import           Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import           Language.Haskell.TH             (Dec, Name, Q, TypeQ)

createPool' :: Config -> IO (Pool Connection)
createPool' config = createPool (connect config) disconnect (dbPoolStripeNum config) (realToFrac (dbPoolKeepTime config)) (dbPoolResourceNum config)

connect :: Config -> IO Connection
connect c =
  connectPostgreSQL $ concat [
  "user='", postgresUser c, "'"
  , "password='", postgresPassword c, "'"
  , "dbname='", postgresDatabase c, "'"
  , "host='", postgresHost c, "'"
  , "port='", postgresPort c, "'"
  ]

connectWithLoadConfig :: IO Connection
connectWithLoadConfig = do
  c <- loadConfig
  connect c

defineEntity :: String -> [Name] -> Q [Dec]
defineEntity =
  defineTableFromDB
    connectWithLoadConfig
    (driverPostgreSQL { typeMap = convTypes })
    "public"

convTypes :: [(String, TypeQ)]
convTypes = []