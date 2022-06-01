{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Api.AppServices (appServices)
import Api.Application (app)
import Api.Config (Port (..), apiConfig, apiPort)
import qualified Api.Config as Config
import CLIOptions (CLIOptions (configPath, jwkPath))
import qualified CLIOptions
import Control.Exception (catch)
import Crypto.JOSE.JWK (JWK)
import Data.ByteString.Char8 (writeFile)
import Data.Function ((&))
import Network.Wai (Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant.Auth.Server (fromSecret, generateSecret, readKey)
import Prelude hiding (writeFile)
import qualified Tagger.Database as DB

main :: IO ()
main = do
  inputOptions <- CLIOptions.parse
  config <- Config.load (configPath inputOptions)
  dbHandle <- DB.new (DB.parseConfig config)
  jsonWebKey <- setupJWK inputOptions
  let port = apiPort . apiConfig $ config

  runApp jsonWebKey port dbHandle

runApp :: JWK -> Port -> DB.Handle -> IO ()
runApp jwk (Port port) dbHandle =
  let services = appServices dbHandle jwk
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
