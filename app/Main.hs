{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.Application (app)
import qualified Infrastructure.Authentication.AuthenticateUser as Auth (AuthenticateUser(AuthenticateUser), authenticateUser, hoistAuthenticateUser)
import Infrastructure.Persistence.PostgresqlContentRepository (postgresContentRepository)
import Tagger.ContentRepository (ContentRepository, hoistContentRepository)

-- base
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)

-- bytestring
import Data.ByteString.Char8 (unpack)

-- hasql
import Hasql.Connection (Connection, acquire)

-- servant
import Servant (Handler)

-- servant-auth-server
import Servant.Auth.Server (generateKey)

-- transformers
import Control.Monad.Trans.Except (runExceptT)

-- warp
import Network.Wai.Handler.Warp (run)

-- TODO: is `fail` the correct thing to use here?
-- TODO: the whole hoistContentRepository stuff could probably be managed in a better way
contentRepository :: Connection -> ContentRepository Handler
contentRepository = hoistContentRepository (liftIO . (=<<) (either (fail . show) pure) . runExceptT) . postgresContentRepository

authenticateUser :: Connection -> Auth.AuthenticateUser Handler
authenticateUser = Auth.hoistAuthenticateUser (liftIO . (=<<) (either (fail . show) pure) . runExceptT) . Auth.AuthenticateUser . Auth.authenticateUser

main:: IO ()
main = do
  -- TODO: use environment variables to pass in connection data
  connection <- acquire "host=localhost port=5432 dbname=tagger user=user password=password"
  key <- generateKey
  either
    (fail . unpack . fromMaybe "unable to connect to the database")
    (\connection' -> run 8080 $ app key (authenticateUser connection') (contentRepository connection'))
    connection
