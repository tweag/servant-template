{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Api.AppServices as AppServices
import Api.Application (app)
import Api.Config (Port (..), apiPort)
import qualified Api.Config as Config
import CLIOptions (CLIOptions (configPath))
import qualified CLIOptions
import Infrastructure.Dependencies (Deps (..))
import qualified Infrastructure.Dependencies as Deps
import qualified Infrastructure.Logging.Logger as Logger
import qualified Middleware
import qualified Network.Wai.Handler.Warp as Warp
import qualified Tagger.JSONWebKey as JWK

main :: IO ()
main = do
  options <- CLIOptions.parse
  appConfig <- Config.load $ configPath options
  key <- JWK.setup options

  Deps.withDeps appConfig $ \Deps {dbHandle, loggerHandle} -> do
    let (Port port) = apiPort . Config.api $ appConfig
        services = AppServices.start dbHandle loggerHandle key
        application = Middleware.apply (app services)

    Logger.logInfo loggerHandle $ "Starting app on port " <> show port <> "."
    Warp.run port application
