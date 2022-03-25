{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Api.Application (app)
import Api.AppServices (appServices)
import Api.Config (api, apiPort, configCodec, connectionString, database, getPort)
import InputOptions (inputOptionsParser, InputOptions (configPath, jwkPath))

-- base
import Control.Exception (catch)
import Data.Maybe (fromMaybe)
import Prelude hiding (writeFile)

-- bytestring
import Data.ByteString.Char8 (writeFile, unpack)

-- hasql
import Hasql.Connection (acquire)

-- jose
import Crypto.JOSE.JWK (JWK)

-- optparse-applicative
import Options.Applicative ((<**>), execParser, helper, info, fullDesc)

-- servant-auth-server
import Servant.Auth.Server (fromSecret, generateSecret, readKey)

-- toml
import Toml (decodeFileExact)

-- wai-extra
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

-- warp
import Network.Wai.Handler.Warp (run)

main:: IO ()
main = do
  -- parse input options
  inputOptions <- execParser $ info (inputOptionsParser <**> helper) fullDesc
  -- extract application configuration from file
  eitherConfig <- decodeFileExact configCodec (configPath inputOptions)
  config <- either (\errors -> fail $ "unable to parse configuration: " <> show errors) pure eitherConfig
  -- acquire the connection to the database
  connection <- acquire $ connectionString (database config)
  either
    (fail . unpack . fromMaybe "unable to connect to the database")
    -- if we were able to connect to the database we run the application
    (\connection' -> do
      -- first we generate a JSON Web Key
      key <- jwtKey (jwkPath inputOptions)
      -- we setup the application services
      let services = appServices connection' key
      -- we retrieve the port from configuration
      let port = getPort . apiPort . api $ config
      -- eventually, we expose the application on the port, using the application services, logging requests on standard output
      run port . logStdoutDev . app $ services)
    connection

jwtKey :: FilePath -> IO JWK
jwtKey path = do
  -- try to retrieve the JWK from file
  catch (readKey path) $ \(_ :: IOError) -> do
    -- if the file does not exist or does not contain a valid key, we generate one
    key <- generateSecret
    -- and we store it
    writeFile path key
    pure $ fromSecret key
