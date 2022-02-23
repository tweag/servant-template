{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Tagger where

-- servant
import Servant.API (type (:>), type (:<|>)(..), Get, Post, JSON)

-- servant-server
import Servant (Server, NoContent (NoContent))

type TaggerAPI
  =    "add-content"  :> Post '[JSON] NoContent
  :<|> "get-contents" :> Get  '[JSON] NoContent

taggerServer :: Server TaggerAPI
taggerServer = pure NoContent :<|> pure NoContent
