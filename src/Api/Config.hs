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

data DatabaseConfig = DatabaseConfig
  { host     :: Host
  , port     :: Port
  , dbname   :: DBName
  , user     :: User
  , password :: Password
  }

connectionString :: DatabaseConfig -> ByteString
connectionString (DatabaseConfig (Host host') port' (DBName dbname') (User user') (Password password')) = pack
  $  "host="     <> unpack host'     <> " "
  <> "port="     <> show port'       <> " "
  <> "dbname="   <> unpack dbname'   <> " "
  <> "user="     <> unpack user'     <> " "
  <> "password=" <> unpack password'

databaseConfigCodec :: TomlCodec DatabaseConfig
databaseConfigCodec = DatabaseConfig
  <$> Toml.diwrap (Toml.text "host")     .= host
  <*> Toml.diwrap (Toml.int  "port")     .= port
  <*> Toml.diwrap (Toml.text "dbname")   .= dbname
  <*> Toml.diwrap (Toml.text "user")     .= user
  <*> Toml.diwrap (Toml.text "password") .= password

newtype ApiConfig = ApiConfig {apiPort :: Port}

apiConfigCodec :: TomlCodec ApiConfig
apiConfigCodec = Toml.diwrap $ Toml.int "port"

data Config = Config
  { database :: DatabaseConfig
  , api      :: ApiConfig
  }

configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.table databaseConfigCodec "database" .= database
  <*> Toml.table apiConfigCodec      "api"      .= api
