module Server (run) where

import API.Config (Config (..), Port (..), apiPort)
import API.Config qualified as Config
import App.Env (Env (..))
import App.Services qualified as AppServices
import Application (mkApp)
import Boot (Handles (..), boot)
import CLIOptions (CLIOptions (configPath))
import CLIOptions qualified
import Infrastructure.Logger qualified as Logger
import Network.Wai.Handler.Warp qualified as Warp
import Tagger.JSONWebKey qualified as JWK

run :: IO ()
run = do
  options <- CLIOptions.parse
  config <- Config.load $ configPath options
  jwkKey <- JWK.setup options

  boot config $ \handles -> do
    let port = config.api.apiPort.getPort
        env = Env {handles, config, jwkKey}
        services = AppServices.start env
        application = mkApp env services

    Logger.logInfo handles.logger $
      "Accepting connections on port " <> show port <> "."

    Warp.run port application
