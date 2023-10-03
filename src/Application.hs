module Application (mkApp) where

import API
import App.Env qualified as App
import App.Services
import Data.Proxy (Proxy (..))
import Middleware qualified
import Network.Wai (Application)
import Servant (Context (EmptyContext, (:.)), Handler, serveWithContext)
import Servant.Auth.Server (defaultCookieSettings, defaultJWTSettings)

mkApp :: App.Env -> Services Handler -> Application
mkApp env services =
  let jwtSettings = defaultJWTSettings env.jwkKey
      app =
        serveWithContext
          (Proxy :: Proxy API)
          (defaultCookieSettings :. jwtSettings :. EmptyContext)
          (mkAPI services)
   in Middleware.apply app
