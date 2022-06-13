module Main where

import qualified Api.AppServices as AppServices
import Api.Application (app)
import Api.Config (Port (..), apiPort)
import qualified Api.Config as Config
import CLIOptions (CLIOptions (configPath))
import qualified CLIOptions
import qualified Infrastructure.Database as DB
import qualified Infrastructure.Logging.Logger as Logger
import qualified Infrastructure.SystemTime as SystemTime
import qualified Middleware
import qualified Network.Wai.Handler.Warp as Warp
import qualified Tagger.JSONWebKey as JWK

main :: IO ()
main = do
  options <- CLIOptions.parse
  appConfig <- Config.load $ configPath options

  SystemTime.withHandle $ \systemTimeHandle ->
    Logger.withHandle systemTimeHandle $ \loggerHandle ->
      DB.withHandle appConfig $ \dbHandle -> do
        jwk <- JWK.setup options

        let (Port port) = apiPort . Config.api $ appConfig
            services = AppServices.start dbHandle loggerHandle jwk
            application = Middleware.apply (app services)

        Logger.logInfo loggerHandle $ "Starting application on port " <> show port
        Warp.run port application
