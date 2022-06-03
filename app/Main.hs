module Main where

import qualified Api.AppServices as AppServices
import Api.Application (app)
import Api.Config (Port (..), apiPort)
import qualified Api.Config as Config
import CLIOptions (CLIOptions (configPath))
import qualified CLIOptions
import qualified Infrastructure.Database as DB
import qualified Middleware
import qualified Network.Wai.Handler.Warp as Warp
import qualified Tagger.JSONWebKey as JWK
import qualified Infrastructure.Database as DB

main :: IO ()
main = do
  options <- CLIOptions.parse
  appConfig <- Config.load $ configPath options
  connection <- DB.acquire appConfig
  DB.withHandle config (\dbHandle -> do
    jwk <- JWK.setup options

    let (Port port) = apiPort . Config.api $ appConfig
        services = AppServices.start connection jwk
        application = Middleware.apply (app services)

    Warp.run port application
