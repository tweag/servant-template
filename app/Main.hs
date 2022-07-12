{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Api.AppServices as AppServices
import Api.Application (app)
import Api.Config (Port (..), apiPort)
import qualified Api.Config as Config
import qualified Middleware
import qualified Network.Wai.Handler.Warp as Warp
import qualified Tagger.JSONWebKey as JWK
import qualified Dependencies as Deps
import Dependencies (Deps(..))
import qualified Infrastructure.Logging.Logger as Logger

main :: IO ()
main = do
  appConfig <- Config.load

  Deps.withDeps appConfig $ \Deps {loggerHandle, dbHandle} -> do
    jwk <- JWK.setup $ Config.jwkPath appConfig

    let (Port port) = apiPort . Config.api $ appConfig
        services = AppServices.start dbHandle loggerHandle jwk
        application = Middleware.apply (app services)

    Logger.logInfo loggerHandle $ "Listening on port " <> show port

    Warp.run port application
