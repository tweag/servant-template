{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Api.Application (app)
import Api.AppServices (appServices)
import Api.Config (api, apiPort, configCodec, getPort)
import InputOptions (inputOptionsParser, InputOptions (configPath, jwkPath))

-- base
import Control.Exception (catch)
import Data.Function ((&))
import Prelude hiding (writeFile)

-- bytestring
import Data.ByteString.Char8 (writeFile)

-- jose
import Crypto.JOSE.JWK (JWK)

-- optparse-applicative
import Options.Applicative ((<**>), execParser, helper, info, fullDesc)

-- servant-auth-server
import Servant.Auth.Server (fromSecret, generateSecret, readKey)

-- toml
import Toml (decodeFileExact)

-- wai-cors
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsRequestHeaders)

-- wai-extra
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

-- warp
import Network.Wai.Handler.Warp (run)

import qualified Infrastructure.Database as DB

main:: IO ()
main = do
  -- parse input options
  inputOptions <- execParser $ info (inputOptionsParser <**> helper) fullDesc
  -- extract application configuration from file
  eitherConfig <- decodeFileExact configCodec (configPath inputOptions)
  config <- either (\errors -> fail $ "unable to parse configuration: " <> show errors) pure eitherConfig
  -- acquire a database handle and yield it to the block
  DB.withHandle config (\dbHandle -> do
    -- first we generate a JSON Web Key
    key <- jwtKey (jwkPath inputOptions)
    -- we start the app dependencies
    let services = appServices dbHandle key
    -- we retrieve the port from configuration
    let port = getPort . apiPort . api $ config
    -- we create our application
    let application
          -- we pass in the required services
          = app services
          -- manage CORS for browser interaction
          & cors (const . Just $ simpleCorsResourcePolicy {corsRequestHeaders = ["Authorization", "Content-Type"]})
          -- we setup logging for the incoming requests
          & logStdoutDev
    -- eventually, we run the application on the port
    run port application
    )


jwtKey :: FilePath -> IO JWK
jwtKey path = do
  -- try to retrieve the JWK from file
  catch (readKey path) $ \(_ :: IOError) -> do
    -- if the file does not exist or does not contain a valid key, we generate one
    key <- generateSecret
    -- and we store it
    writeFile path key
    pure $ fromSecret key
