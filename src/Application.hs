module Application (mkApp) where

import API
import App.Env qualified as App
import App.Error (handleAppError)
import App.Services
import AppM (AppM, runApp)
import Data.Proxy (Proxy (..))
import Middleware qualified
import Network.Wai (Application)
import Servant (Context (EmptyContext, (:.)), Handler, HasServer (hoistServerWithContext), serveWithContext)
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings)

mkApp :: App.Env -> Services AppM -> Application
mkApp env services =
  let api = Proxy @API
      context = defaultCookieSettings :. defaultJWTSettings env.jwkKey :. EmptyContext
      app =
        serveWithContext api context $
          hoistServerWithContext
            api
            (Proxy @'[CookieSettings, JWTSettings])
            (run env)
            (mkAPI services)
   in Middleware.apply app

run :: App.Env -> AppM a -> Handler a
run env computation = do
  runApp env computation >>= \case
    Right a -> pure a
    Left e -> handleAppError env.handles.logger e
