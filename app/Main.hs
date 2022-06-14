module Main where

import qualified Api.AppServices as AppServices
import Api.Application (app)
import Api.Config (Port (..), apiPort)
import qualified Api.Config as Config
import CLIOptions (CLIOptions (configPath))
import qualified CLIOptions
import qualified Dependencies as Deps (database, logger, start)
import qualified Infrastructure.Logging.Logger as Logger
import qualified Middleware
import qualified Network.Wai.Handler.Warp as Warp
import qualified Tagger.JSONWebKey as JWK

main :: IO ()
main = do
  options <- CLIOptions.parse
  appConfig <- Config.load $ configPath options
  jwk <- JWK.setup options
  deps <- Deps.start appConfig

  let (Port port) = apiPort . Config.api $ appConfig
      (loggerHandle, dbHandle) = (Deps.logger deps, Deps.database deps)
      services = AppServices.start dbHandle loggerHandle jwk
      application = Middleware.apply (app services)

  Logger.logInfo loggerHandle $ "Starting application on port " <> show port
  Warp.run port application
