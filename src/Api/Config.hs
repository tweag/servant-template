{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Api.Config where

import qualified CLIOptions as Opts
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import qualified Data.Text as Text
import Toml (Key, TomlCodec, decodeFileExact, diwrap, int, table, text, textBy, (.=))

-- |
-- The whole config needed by the application
data Config = Config
  { database :: DatabaseConfig,
    api :: ApiConfig
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
-- The configuration parameters needed to run the API
data ApiConfig = ApiConfig
  { apiPort :: Port,
    apiJwkPath :: FilePath
  }

newtype Host = Host {getHost :: Text}

newtype Port = Port {getPort :: Int}
  deriving newtype (Show)

newtype DBName = DBName {getDBName :: Text}

newtype User = User {getUser :: Text}

newtype Password = Password {getPassword :: Text}

-- |
-- Reads configuration file at given filepath and keeps options passed
-- in the command line
load :: (MonadIO m, MonadFail m) => m Config
load = do
  options <- liftIO Opts.parse
  let path = Opts.configPath options
  config <- parseConfig path

  pure $ mergeCLIOptions config options

-- |
-- Attempts to parse configuration at given path
parseConfig :: (MonadIO m, MonadFail m) => FilePath -> m Config
parseConfig path = do
  eitherConfig <- decodeFileExact configCodec path
  either
    (\errors -> fail $ "unable to parse configuration: " <> show errors)
    pure
    eitherConfig

-- |
-- Merges parsed configuration with options, prioritizing
-- values passed in CLI
mergeCLIOptions :: Config -> Opts.CLIOptions -> Config
mergeCLIOptions config options =
  maybe
    config
    (\jwk -> config {api = (api config) {apiJwkPath = jwk}})
    (Opts.jwkPath options)

-- | Convenience alias for JSON Web Key path
jwkPath :: Config -> FilePath
jwkPath = apiJwkPath . api

-- |
-- A bidirectional codec for 'DatabaseConfig'
databaseConfigCodec :: TomlCodec DatabaseConfig
databaseConfigCodec =
  DatabaseConfig
    <$> Toml.diwrap (Toml.text "host") .= host
    <*> Toml.diwrap (Toml.int "port") .= port
    <*> Toml.diwrap (Toml.text "dbname") .= dbname
    <*> Toml.diwrap (Toml.text "user") .= user
    <*> Toml.diwrap (Toml.text "password") .= password

-- |
-- A bidirectional codec for 'ApiConfig'
apiConfigCodec :: TomlCodec ApiConfig
apiConfigCodec =
  ApiConfig
    <$> Toml.diwrap (Toml.int "port") .= apiPort
    <*> Toml.diwrap (filePathCodec "jwk_path") .= apiJwkPath

-- |
-- A bidirectional codec for 'FilePath'
filePathCodec :: Toml.Key -> TomlCodec FilePath
filePathCodec =
  textBy Text.pack (Right . Text.unpack)

-- |
-- A bidirectional codec for 'Config'
configCodec :: TomlCodec Config
configCodec =
  Config
    <$> Toml.table databaseConfigCodec "database" .= database
    <*> Toml.table apiConfigCodec "api" .= api
