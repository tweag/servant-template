{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.Application (app)
import qualified Infrastructure.Authentication.AuthenticateUser as Auth (AuthenticateUser(AuthenticateUser), authenticateUser, hoistAuthenticateUser)
import Infrastructure.Persistence.PostgresContentRepository (postgresContentRepository)
import Infrastructure.Persistence.PostgresUserRepository (postgresUserRepository)
import Tagger.ContentRepository (ContentRepository, hoistContentRepository)
import Tagger.UserRepository (UserRepository, hoistUserRepository)

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
import Control.Monad.Trans.Except (ExceptT, runExceptT)

-- warp
import Network.Wai.Handler.Warp (run)

eitherTtoHandler :: Show e => ExceptT e IO a -> Handler a
eitherTtoHandler = liftIO . (=<<) (either (fail . show) pure) . runExceptT

-- TODO: is `fail` the correct thing to use here?
-- TODO: the whole hoistContentRepository stuff could probably be managed in a better way
contentRepository :: Connection -> ContentRepository Handler
contentRepository = hoistContentRepository eitherTtoHandler . postgresContentRepository

authenticateUser :: Connection -> Auth.AuthenticateUser Handler
authenticateUser = Auth.hoistAuthenticateUser eitherTtoHandler . Auth.AuthenticateUser . Auth.authenticateUser

userRepository :: Connection -> UserRepository Handler
userRepository = hoistUserRepository eitherTtoHandler . postgresUserRepository

main:: IO ()
main = do
  -- TODO: use environment variables to pass in connection data
  connection <- acquire "host=localhost port=5432 dbname=tagger user=user password=password"
  key <- generateKey
  either
    (fail . unpack . fromMaybe "unable to connect to the database")
    (\connection' -> run 8080 $ app key (authenticateUser connection') (userRepository connection') (contentRepository connection'))
    connection
