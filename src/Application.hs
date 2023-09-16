module Application (mkApp', mkApp) where

import API
import API.AppServices as AppServices
import App.Env qualified as App
import Data.Proxy (Proxy (..))
import Middleware qualified
import Network.Wai (Application)
import Servant (Context (EmptyContext, (:.)), serveWithContext)
import Servant.Auth.Server (defaultCookieSettings)

mkApp' :: App.Env -> Application
mkApp' context =
  let services = AppServices.start context
      app = mkApp services
   in Middleware.apply app

mkApp :: AppServices -> Application
mkApp appServices =
  serveWithContext
    (Proxy :: Proxy API)
    (defaultCookieSettings :. jwtSettings appServices :. EmptyContext)
    (mkAPI appServices)
