{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Healthcheck where

import Servant (NoContent (NoContent), Server)
import Servant.API (Get, JSON, type (:>))

-- |
-- A single endpoint to check the liveness of the application
type HealthcheckAPI = "healthcheck" :> Get '[JSON] NoContent

healthcheckServer :: Server HealthcheckAPI
healthcheckServer = pure NoContent
