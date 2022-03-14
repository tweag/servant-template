{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.Application (app)
import Api.AppServices (appServices)
import Api.Config (api, apiPort, configCodec, connectionString, database, getPort)

-- base
import Data.Maybe (fromMaybe)

-- bytestring
import Data.ByteString.Char8 (unpack)

-- hasql
import Hasql.Connection (acquire)

-- servant-auth-server
import Servant.Auth.Server (generateKey)

-- toml
import Toml (decodeFileExact)

-- wai-extra
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

-- warp
import Network.Wai.Handler.Warp (run)

main:: IO ()
main = do
  -- extract application configuration from `config.toml` file
  eitherConfig <- decodeFileExact configCodec "./config.toml"
  config <- either (\errors -> fail $ "unable to parse configuration: " <> show errors) pure eitherConfig
  -- acquire the connection to the database
  connection <- acquire $ connectionString (database config)
  either
    (fail . unpack . fromMaybe "unable to connect to the database")
    -- if we were able to connect to the database we run the application
    (\connection' -> do
      -- first we generate a JSON Web Key
      key <- generateKey
      -- we setup the application services
      let services = appServices connection' key
      -- we retrieve the port from configuration
      let port = getPort . apiPort . api $ config
      -- eventually, we expose the application on the port, using the application services, logging requests on standard output
      run port . logStdoutDev . app $ services)
    connection
