{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Config(Config, api, apiPort, configCodec, connectionString, dbConfig, getPort) where

-- bystestring
import Data.ByteString.Char8 (ByteString, pack)

-- text
import Data.Text (Text, unpack)

-- toml
import Toml (TomlCodec, (.=), text, table)
import qualified Toml (diwrap, int)

newtype Host = Host {getHost :: Text}

newtype Port = Port {getPort :: Int}
  deriving newtype Show

newtype DBName = DBName {getDBName :: Text}

newtype User = User {getUser :: Text}

newtype Password = Password {getPassword :: Text}

-- |
-- The configuration parameters needed to connect to a database
data DatabaseConfig = DatabaseConfig
  { host     :: Host
  , port     :: Port
  , dbname   :: DBName
  , user     :: User
  , password :: Password
  }

-- |
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
-- The configuration parameters needed to expose the API
newtype ApiConfig = ApiConfig {apiPort :: Port}

-- |
-- A bidirectional codec for 'ApiConfig'
apiConfigCodec :: TomlCodec ApiConfig
apiConfigCodec = Toml.diwrap $ Toml.int "port"

-- |
-- The whole config needed by the application
data Config = Config
  { database :: DatabaseConfig
  , api      :: ApiConfig
  }

-- |
-- More explicit alias for the database field
dbConfig :: Config -> DatabaseConfig
dbConfig = database

-- |
-- A bidirectional codec for 'Config'
configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.table databaseConfigCodec "database" .= database
  <*> Toml.table apiConfigCodec      "api"      .= api
