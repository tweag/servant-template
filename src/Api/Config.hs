{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Config where

import Data.ByteString.Char8 (ByteString, pack)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text, unpack)
import Toml (TomlCodec, (.=), text, table, decodeFileExact, diwrap, int)

data Config = Config
  { dbConfig :: DatabaseConfig
  , apiConfig      :: ApiConfig
  }

newtype ApiConfig = ApiConfig {apiPort :: Port}

data DatabaseConfig = DatabaseConfig
  { host     :: Host
  , port     :: Port
  , dbname   :: DBName
  , user     :: User
  , password :: Password
  }

newtype Host = Host {getHost :: Text}

newtype Port = Port {getPort :: Int}
  deriving newtype Show

newtype DBName = DBName {getDBName :: Text}

newtype User = User {getUser :: Text}

newtype Password = Password {getPassword :: Text}

-- Compute the connection string given a 'DatabaseConfig'
connectionString :: DatabaseConfig -> ByteString
connectionString DatabaseConfig{host, port, dbname, user, password} = pack
  $  "host="     <> unpack (getHost host)     <> " "
  <> "port="     <> show port                 <> " "
  <> "dbname="   <> unpack (getDBName dbname) <> " "
  <> "user="     <> unpack (getUser user)     <> " "
  <> "password=" <> unpack (getPassword password)

-- |
-- A bidirectional codec for 'DatabaseConfig'
databaseConfigCodec :: TomlCodec DatabaseConfig
databaseConfigCodec = DatabaseConfig
  <$> Toml.diwrap (Toml.text "host")     .= host
  <*> Toml.diwrap (Toml.int  "port")     .= port
  <*> Toml.diwrap (Toml.text "dbname")   .= dbname
  <*> Toml.diwrap (Toml.text "user")     .= user
  <*> Toml.diwrap (Toml.text "password") .= password

-- |
-- A bidirectional codec for 'ApiConfig'
apiConfigCodec :: TomlCodec ApiConfig
apiConfigCodec = Toml.diwrap $ Toml.int "port"

-- |
-- A bidirectional codec for 'Config'
configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.table databaseConfigCodec "database" .= dbConfig
  <*> Toml.table apiConfigCodec      "api"      .= apiConfig

load :: (MonadIO m, MonadFail m) => FilePath -> m Config
load path = do
  eitherConfig <- decodeFileExact configCodec path
  either (\errors -> fail $ "unable to parse configuration: " <> show errors) pure eitherConfig

