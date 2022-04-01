{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Api.AppServices where

import qualified Infrastructure.Authentication.AuthenticateUser as Auth (AuthenticateUser(AuthenticateUser), AuthenticationError(AuthenticationQueryError), authenticateUser, hoistAuthenticateUser)
import Infrastructure.Authentication.PasswordManager (PasswordManager, PasswordManagerError(..), hoistPasswordManager, bcryptPasswordManager)
import Infrastructure.Logging.Logger (messageLogger, provideContext)
import Infrastructure.Persistence.PostgresContentRepository (postgresContentRepository)
import Infrastructure.Persistence.PostgresUserRepository (postgresUserRepository, UserRepositoryError (DuplicateUserName))
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
import Hasql.Session (QueryError)

-- jose
import Crypto.JOSE.JWK (JWK)

-- mtl
import Control.Monad.Except (throwError)

-- servant-auth-server
import Servant.Auth.Server (JWTSettings, defaultJWTSettings)

-- servant-server
import Servant (Handler, err500, err401, err403)

-- transformers
import Control.Monad.Trans.Except (ExceptT, runExceptT)

-- |
-- Collection of services needed by the application to work
data AppServices = AppServices
  { jwtSettings       :: JWTSettings
  , passwordManager   :: PasswordManager Handler
  , contentRepository :: ContentRepository Handler
  , userRepository    :: UserRepository Handler
  , authenticateUser  :: Auth.AuthenticateUser Handler
  }

-- |
-- 'LogAction' which accepts any 'Show'able data type and a 'Severity'
type SeverityLogger = forall a. Show a => LogAction Handler (Severity, a)

-- |
-- Lifts a computation from 'ExceptT e IO' to 'Handler a' using the provided 'handleError' function
eitherTToHandler :: (e -> Handler a) -> ExceptT e IO a -> Handler a
eitherTToHandler handleError = either handleError pure <=< liftIO . runExceptT

-- |
-- Lifts a 'ContentRepository' fo the 'Handler' monad, handling all errors by logging them and returning a 500 response
connectedContentRepository :: SeverityLogger -> ContentRepository (ExceptT QueryError IO) -> ContentRepository Handler
connectedContentRepository log = hoistContentRepository (eitherTToHandler $ (>> throwError err500) . (log <&) . (Error ,))

-- |
-- Lifts a 'UserRepository' fo the 'Handler' monad, handling all errors by logging them and returning a 500 response
connectedUserRepository :: SeverityLogger -> UserRepository (ExceptT UserRepositoryError IO) -> UserRepository Handler
connectedUserRepository log = hoistUserRepository $ eitherTToHandler handleUserRepositoryError-- (eitherTToHandler $ (>> throwError err500) . (log <&) . (Error ,))
  where
    handleUserRepositoryError :: UserRepositoryError -> Handler a
    -- If the database error concerns a duplicate user, we return a 403 response
    handleUserRepositoryError (DuplicateUserName e) = do
      log <& (Warning, DuplicateUserName e)
      throwError err403
    -- Otherwise, we return a 500 response
    handleUserRepositoryError e                     = do
      log <& (Error, e)
      throwError err500

-- |
-- Creates an 'AuthenticateUser' service injecting its dependencies and handling errors
connectedAuthenticateUser :: SeverityLogger -> UserRepository (ExceptT UserRepositoryError IO) -> PasswordManager Handler -> Auth.AuthenticateUser Handler
connectedAuthenticateUser log userRepository' passwordManager'
  = Auth.hoistAuthenticateUser (eitherTToHandler handleAuthenticationError)
  . Auth.AuthenticateUser
  $ Auth.authenticateUser userRepository' passwordManager'
    where
      handleAuthenticationError :: Auth.AuthenticationError -> Handler a
      -- If there was an error at the database level, we return a 500 response
      handleAuthenticationError (Auth.AuthenticationQueryError e) = do
        log <& (Error, Auth.AuthenticationQueryError e)
        throwError err500
        -- In other cases, there was an authentication error and we return a 401 response
      handleAuthenticationError e = do
        log <& (Warning, e)
        throwError err401

-- |
-- Creates a 'PasswordManager' service injecting its dependencies and handling errors
encryptedPasswordManager :: SeverityLogger -> JWTSettings -> PasswordManager Handler
encryptedPasswordManager log = hoistPasswordManager (eitherTToHandler handlePasswordManagerError) . bcryptPasswordManager
  where
    handlePasswordManagerError :: PasswordManagerError -> Handler a
    -- If there was a failure during password hashing, we return a 500 response
    handlePasswordManagerError FailedHashing = do
      log <& (Error, FailedHashing)
      throwError err500
    -- In other cases, we return a 401 response
    handlePasswordManagerError (FailedJWTCreation e) = do
      log <& (Error, FailedJWTCreation e)
      throwError err401

-- |
-- Creates all the services needed by the application, creating a different contexts for the logger of each service
appServices :: Connection -> JWK -> AppServices
appServices connection key =
  let
    passwordManager' = encryptedPasswordManager (provideContext "PasswordManager" messageLogger) $ defaultJWTSettings key
    dbUserRepository = postgresUserRepository connection
  in  AppServices
    { jwtSettings       = defaultJWTSettings key
    , passwordManager   = passwordManager'
    , contentRepository = connectedContentRepository (provideContext "ContentRepository" messageLogger) (postgresContentRepository connection)
    , userRepository    = connectedUserRepository (provideContext "UserRepository" messageLogger) dbUserRepository
    , authenticateUser  = connectedAuthenticateUser (provideContext "AuthenticateUser" messageLogger) dbUserRepository passwordManager'
    }
