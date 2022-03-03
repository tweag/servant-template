{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.Application (app)
import Api.AppServices (AppServices(..))
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

-- jose
import Crypto.JOSE.JWK (JWK)

-- servant
import Servant (Handler)

-- servant-auth-server
import Servant.Auth.Server (defaultJWTSettings, generateKey)

-- transformers
import Control.Monad.Trans.Except (ExceptT, runExceptT)

-- warp
import Network.Wai.Handler.Warp (run)

-- TODO: is `fail` the correct thing to use here?
eitherTtoHandler :: Show e => ExceptT e IO a -> Handler a
eitherTtoHandler = liftIO . (=<<) (either (fail . show) pure) . runExceptT

-- TODO: the whole hoist stuff could probably be managed in a better way
connectedContentRepository :: Connection -> ContentRepository Handler
connectedContentRepository = hoistContentRepository eitherTtoHandler . postgresContentRepository

connectedUserRepository :: Connection -> UserRepository Handler
connectedUserRepository = hoistUserRepository eitherTtoHandler . postgresUserRepository

connectedAuthenticateUser :: Connection -> Auth.AuthenticateUser Handler
connectedAuthenticateUser = Auth.hoistAuthenticateUser eitherTtoHandler . Auth.AuthenticateUser . Auth.authenticateUser

appServices :: Connection -> JWK -> AppServices
appServices connection key = AppServices
  { jwtSettings       = defaultJWTSettings key
  , contentRepository = connectedContentRepository connection
  , userRepository    = connectedUserRepository connection
  , authenticateUser  = connectedAuthenticateUser connection
  }

main:: IO ()
main = do
  -- TODO: retrieve connection data from configuration file
  connection <- acquire "host=localhost port=5432 dbname=tagger user=user password=password"
  key <- generateKey
  either
    (fail . unpack . fromMaybe "unable to connect to the database")
    (\connection' -> run 8080 $ app (appServices connection' key))
    connection
