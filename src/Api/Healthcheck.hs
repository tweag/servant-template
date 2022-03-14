{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Healthcheck where

-- servant
import Servant.API (type (:>), Get, JSON)

-- servant-server
import Servant (Server, NoContent (NoContent))

-- |
-- A single endpoint to check the liveness of the application
type HealthcheckAPI = "healthcheck" :> Get '[JSON] NoContent

healthcheckServer :: Server HealthcheckAPI
healthcheckServer = pure NoContent
