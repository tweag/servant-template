{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Tagger where

import Tagger.Content (Content)
import Tagger.Tag (Tag)

-- base
import Control.Monad.IO.Class (liftIO)

-- servant
import Servant.API (type (:>), type (:<|>)(..), Get, Post, JSON, ReqBody)

-- servant-server
import Servant (Server, Handler)

-- uuid
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)

type TaggerAPI
  =    "add-content"  :> ReqBody '[JSON] Content :> Post '[JSON] UUID
  :<|> "get-contents" :> ReqBody '[JSON] [Tag]   :> Get  '[JSON] [Content]

addContentHandler :: Content -> Handler UUID
addContentHandler _ = liftIO nextRandom

getContentsHandler :: [Tag] -> Handler [Content]
getContentsHandler _ = pure []

taggerServer :: Server TaggerAPI
taggerServer = addContentHandler :<|> getContentsHandler
