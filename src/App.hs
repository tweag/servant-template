module App where

import API.AppServices as AppServices
import API.Application (app)
import API.Config (Port (..), apiPort)
import API.Config qualified as Config
import CLIOptions (CLIOptions (configPath))
import CLIOptions qualified
import Dependencies (Deps (..))
import Dependencies qualified as Deps
import Infrastructure.Logging.Logger qualified as Logger
import Middleware qualified
import Network.Wai.Handler.Warp qualified as Warp
import Tagger.JSONWebKey qualified as JWK

run :: IO ()
run = do
  options <- CLIOptions.parse
  appConfig <- Config.load $ configPath options
  key <- JWK.setup options

  Deps.withDeps appConfig $ \Deps {dbHandle, loggerHandle} -> do
    let (Port port) = appConfig.api.apiPort
        services = AppServices.start dbHandle loggerHandle key
        application = Middleware.apply (app services)

    Logger.logInfo loggerHandle $ "Starting app on port " <> show port <> "."
    Warp.run port application
