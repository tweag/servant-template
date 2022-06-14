{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Api.AppServices as AppServices
import Api.Application (app)
import Api.Config (Port (..), apiPort)
import qualified Api.Config as Config
import CLIOptions (CLIOptions (configPath))
import qualified CLIOptions
import qualified Middleware
import qualified Network.Wai.Handler.Warp as Warp
import qualified Tagger.JSONWebKey as JWK
import qualified Dependencies as Deps
import Dependencies (Deps(..))

main :: IO ()
main = do
  options <- CLIOptions.parse
  appConfig <- Config.load $ configPath options

  Deps.withDeps appConfig $ \Deps {loggerHandle, dbHandle} -> do
    jwk <- JWK.setup options

    let (Port port) = apiPort . Config.api $ appConfig
        services = AppServices.start dbHandle loggerHandle jwk
        application = Middleware.apply (app services)

    Warp.run port application
