{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Tagger where

-- servant
import Servant.API (type (:>), Get, JSON)

-- servant-server
import Servant (Server, NoContent (NoContent))

type TaggerAPI
  = "healthcheck" :> Get '[JSON] NoContent 

taggerServer :: Server TaggerAPI
taggerServer = pure NoContent
