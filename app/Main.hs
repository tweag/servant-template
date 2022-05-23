{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Api.AppServices (appServices)
import Api.Application (app)
import Api.Config (Config, apiConfig, apiPort, connectionString, dbConfig, getPort)
import qualified Api.Config as Config
import CLIOptions (CLIOptions (configPath, jwkPath))
import qualified CLIOptions
import Control.Exception (catch)
import Crypto.JOSE.JWK (JWK)
import Data.ByteString.Char8 (unpack, writeFile)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Hasql.Connection (Connection, ConnectionError, acquire)
import Network.Wai (Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant.Auth.Server (fromSecret, generateSecret, readKey)
import Prelude hiding (writeFile)

main :: IO ()
main = do
  inputOptions <- CLIOptions.parse
  config <- Config.load (configPath inputOptions)
  dbConnection <- setupDBConnection config
  jsonWebKey <- setupJWK inputOptions

  either
    (fail . unpack . fromMaybe "unable to connect to the database")
    (runApp jsonWebKey config)
    dbConnection

setupDBConnection :: Config -> IO (Either ConnectionError Connection)
setupDBConnection =
  acquire . connectionString . dbConfig

runApp :: JWK -> Config -> Connection -> IO ()
runApp jwk config dbConnection =
  let services = appServices dbConnection jwk
      port = getPort . apiPort . apiConfig $ config
      application = app services & corsMiddleware & logStdoutDev
   in Warp.run port application

corsMiddleware :: Network.Wai.Middleware
corsMiddleware =
  let headers = ["Authorization", "Content-Type"]
   in cors (const . Just $ simpleCorsResourcePolicy {corsRequestHeaders = headers})

setupJWK :: CLIOptions -> IO JWK
setupJWK =
  jwtKey . jwkPath

jwtKey :: FilePath -> IO JWK
jwtKey path = do
  -- try to retrieve the JWK from file
  catch (readKey path) $ \(_ :: IOError) -> do
    -- if the file does not exist or does not contain a valid key, we generate one
    key <- generateSecret
    -- and we store it
    writeFile path key
    pure $ fromSecret key
