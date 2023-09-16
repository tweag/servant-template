module API.Healthcheck (API, api) where

import Servant (NoContent (NoContent), Server)
import Servant.API (Get, JSON, type (:>))

-- |
-- A single endpoint to check the liveness of the application
type API = "healthcheck" :> Get '[JSON] NoContent

api :: Server API
api = pure NoContent
