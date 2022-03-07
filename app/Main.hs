{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.Application (app)
import Api.AppServices (appServices)
import Api.Config (api, apiPort, asInt, configCodec, connectionString, database)

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

-- warp
import Network.Wai.Handler.Warp (run)

main:: IO ()
main = do
  eitherConfig <- decodeFileExact configCodec "./config.toml"
  config <- either (\errors -> fail $ "unable to parse configuration: " <> show errors) pure eitherConfig
  connection <- acquire $ connectionString (database config)
  either
    (fail . unpack . fromMaybe "unable to connect to the database")
    (\connection' -> generateKey >>= run (asInt . apiPort . api $ config) . app . appServices connection')
    connection
