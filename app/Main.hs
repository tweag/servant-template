module Main where

import qualified Api.AppServices as AppServices
import Api.Application (app)
import Api.Config (Port (..), apiConfig, apiPort)
import qualified Api.Config as Config
import CLIOptions (CLIOptions (configPath))
import qualified CLIOptions
import qualified Middleware
import qualified Network.Wai.Handler.Warp as Warp
import qualified Tagger.Database as DB
import qualified Tagger.JSONWebKey as JWK

main :: IO ()
main = do
  options <- CLIOptions.parse
  appConfig <- Config.load $ configPath options
  dbHandle <- DB.new $ DB.parseConfig appConfig
  jwk <- JWK.setup options

  let (Port port) = apiPort . apiConfig $ appConfig
      services = AppServices.start dbHandle jwk
      application = Middleware.withMiddlewares (app services)

  Warp.run port application
