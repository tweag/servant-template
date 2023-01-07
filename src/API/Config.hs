{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module API.Config where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Char8 (ByteString, pack)
import Data.Text (Text, unpack)
import Toml (TomlCodec, decodeFileExact, diwrap, int, table, text, (.=))

-- |
-- The whole config needed by the application
data Config = Config
  { database :: DatabaseConfig,
    api :: ApiConfig
  }

-- |
-- The configuration parameters needed to expose the API
newtype ApiConfig = ApiConfig
  { apiPort :: Port
  }

-- |
-- The configuration parameters needed to connect to a database
data DatabaseConfig = DatabaseConfig
  { host :: Host,
    port :: Port,
    dbname :: DBName,
    user :: User,
    password :: Password
  }

-- |
-- Reads configuration file at given filepath
load :: (MonadIO m, MonadFail m) => FilePath -> m Config
load path = do
  eitherConfig <- decodeFileExact configCodec path
  either (\errors -> fail $ "unable to parse configuration: " <> show errors) pure eitherConfig

newtype Host = Host {getHost :: Text}

newtype Port = Port {getPort :: Int}
  deriving newtype (Show)

newtype DBName = DBName {getDBName :: Text}

newtype User = User {getUser :: Text}

newtype Password = Password {getPassword :: Text}

-- |
-- Compute the connection string given a 'DatabaseConfig'
connectionString :: DatabaseConfig -> ByteString
connectionString DatabaseConfig {host, port, dbname, user, password} =
  pack $
    "host="
      <> unpack (getHost host)
      <> " "
      <> "port="
      <> show port
      <> " "
      <> "dbname="
      <> unpack (getDBName dbname)
      <> " "
      <> "user="
      <> unpack (getUser user)
      <> " "
      <> "password="
      <> unpack (getPassword password)

-- |
-- A bidirectional codec for 'DatabaseConfig'
databaseConfigCodec :: TomlCodec DatabaseConfig
databaseConfigCodec =
  DatabaseConfig
    <$> Toml.diwrap (Toml.text "host")
    .= host
    <*> Toml.diwrap (Toml.int "port")
    .= port
    <*> Toml.diwrap (Toml.text "dbname")
    .= dbname
    <*> Toml.diwrap (Toml.text "user")
    .= user
    <*> Toml.diwrap (Toml.text "password")
    .= password

-- |
-- A bidirectional codec for 'ApiConfig'
apiConfigCodec :: TomlCodec ApiConfig
apiConfigCodec = Toml.diwrap $ Toml.int "port"

-- |
-- A bidirectional codec for 'Config'
configCodec :: TomlCodec Config
configCodec =
  Config
    <$> Toml.table databaseConfigCodec "database"
    .= database
    <*> Toml.table apiConfigCodec "api"
    .= api
