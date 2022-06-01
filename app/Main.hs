{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.AppServices (appServices)
import Api.Application (app)
import Api.Config (Port (..), apiConfig, apiPort)
import qualified Api.Config as Config
import CLIOptions (CLIOptions (configPath))
import qualified CLIOptions
import Data.Function ((&))
import Network.Wai (Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Tagger.Database as DB
import qualified Tagger.JWK as JWK

main :: IO ()
main = do
  inputOptions <- CLIOptions.parse
  config <- Config.load (configPath inputOptions)
  dbHandle <- DB.new (DB.parseConfig config)
  jsonWebKey <- JWK.setup inputOptions
  let port = apiPort . apiConfig $ config

  runApp jsonWebKey port dbHandle

runApp :: JWK.JWK -> Port -> DB.Handle -> IO ()
runApp jwk (Port port) dbHandle =
  let services = appServices dbHandle jwk
      application = app services & corsMiddleware & logStdoutDev
   in Warp.run port application

corsMiddleware :: Network.Wai.Middleware
corsMiddleware =
  let headers = ["Authorization", "Content-Type"]
   in cors (const . Just $ simpleCorsResourcePolicy {corsRequestHeaders = headers})
