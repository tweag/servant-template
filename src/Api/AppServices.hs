{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Api.AppServices where

import qualified Infrastructure.Authentication.AuthenticateUser as Auth (AuthenticateUser(AuthenticateUser), AuthenticationError(AuthenticationQueryError), authenticateUser, hoistAuthenticateUser)
import Infrastructure.Authentication.PasswordManager (PasswordManager, PasswordManagerError(..), hoistPasswordManager, bcryptPasswordManager)
import Infrastructure.Logging.Logger (messageLogger, provideContext)
import Infrastructure.Persistence.PostgresContentRepository (postgresContentRepository)
import Infrastructure.Persistence.PostgresUserRepository (postgresUserRepository)
import Tagger.ContentRepository (ContentRepository, hoistContentRepository)
import Tagger.UserRepository (UserRepository, hoistUserRepository)

-- base
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Prelude hiding (log)

-- co-log-core
import Colog.Core ((<&), LogAction, Severity(..))

-- hasql
import Hasql.Connection (Connection)

-- jose
import Crypto.JOSE.JWK (JWK)

-- mtl
import Control.Monad.Except (throwError)

-- servant-auth-server
import Servant.Auth.Server (JWTSettings, defaultJWTSettings)

-- servant-server
import Servant (Handler, err500, err401)

-- transformers
import Control.Monad.Trans.Except (ExceptT, runExceptT)

data AppServices = AppServices
  { jwtSettings       :: JWTSettings
  , passwordManager   :: PasswordManager Handler
  , contentRepository :: ContentRepository Handler
  , userRepository    :: UserRepository Handler
  , authenticateUser  :: Auth.AuthenticateUser Handler
  }

type SeverityLogger = forall a. Show a => LogAction Handler (Severity, a)

eitherTToHandler :: (e -> Handler a) -> ExceptT e IO a -> Handler a
eitherTToHandler handleError = either handleError pure <=< liftIO . runExceptT

connectedContentRepository :: SeverityLogger -> Connection -> ContentRepository Handler
connectedContentRepository log = hoistContentRepository (eitherTToHandler $ ((log <&) . (Error,)) >> const (throwError err500)) . postgresContentRepository

connectedUserRepository :: SeverityLogger -> Connection -> UserRepository Handler
connectedUserRepository log = hoistUserRepository (eitherTToHandler $ ((log <&) . (Error,)) >> const (throwError err500)) . postgresUserRepository

connectedAuthenticateUser :: SeverityLogger -> PasswordManager Handler -> Connection -> Auth.AuthenticateUser Handler
connectedAuthenticateUser log passwordManager' = Auth.hoistAuthenticateUser (eitherTToHandler handleAuthenticationError) . Auth.AuthenticateUser . Auth.authenticateUser passwordManager'
  where
    handleAuthenticationError :: Auth.AuthenticationError -> Handler a
    handleAuthenticationError (Auth.AuthenticationQueryError e) = do
      log <& (Error, Auth.AuthenticationQueryError e)
      throwError err500
    handleAuthenticationError e = do
      log <& (Warning, e)
      throwError err401

encryptedPasswordManager :: SeverityLogger -> JWTSettings -> PasswordManager Handler
encryptedPasswordManager log = hoistPasswordManager (eitherTToHandler handlePasswordManagerError) . bcryptPasswordManager
  where
    handlePasswordManagerError :: PasswordManagerError -> Handler a
    handlePasswordManagerError FailedHashing = do
      log <& (Error, FailedHashing)
      throwError err500
    handlePasswordManagerError (FailedJWTCreation e) = do
      log <& (Error, FailedJWTCreation e)
      throwError err401

appServices :: Connection -> JWK -> AppServices
appServices connection key =
  let
    passwordManager' = encryptedPasswordManager (provideContext "PasswordManager" messageLogger) $ defaultJWTSettings key
  in  AppServices
    { jwtSettings       = defaultJWTSettings key
    , passwordManager   = passwordManager'
    , contentRepository = connectedContentRepository (provideContext "ContentRepository" messageLogger) connection
    , userRepository    = connectedUserRepository (provideContext "UserRepository" messageLogger) connection
    , authenticateUser  = connectedAuthenticateUser (provideContext "AuthenticateUser" messageLogger) passwordManager' connection
    }
