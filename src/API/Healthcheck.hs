module API.Healthcheck (API, api) where

import AppM (AppM)
import Servant (NoContent (NoContent), ServerT)
import Servant.API (Get, JSON)

-- |
-- A single endpoint to check the liveness of the application
type API = Get '[JSON] NoContent

api :: ServerT API AppM
api = pure NoContent
