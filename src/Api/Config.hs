{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Config where

-- bystestring
import Data.ByteString.Char8 (ByteString, pack)

-- text
import Data.Text (Text, unpack)

-- toml
import Toml (TomlCodec, (.=), text, table)
import qualified Toml (diwrap, int)

newtype Host = Host Text

newtype Port = Port {asInt :: Int}
  deriving newtype Show

newtype DBName = DBName Text

newtype User = User Text

newtype Password = Password Text

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
connectionString (DatabaseConfig (Host host') port' (DBName dbname') (User user') (Password password')) = pack
  $  "host="     <> unpack host'     <> " "
  <> "port="     <> show port'       <> " "
  <> "dbname="   <> unpack dbname'   <> " "
  <> "user="     <> unpack user'     <> " "
  <> "password=" <> unpack password'

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
-- A bidirectional codec for 'Config'
configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.table databaseConfigCodec "database" .= database
  <*> Toml.table apiConfigCodec      "api"      .= api
