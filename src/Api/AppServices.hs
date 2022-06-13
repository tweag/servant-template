{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Api.AppServices where

import qualified Infrastructure.Authentication.AuthenticateUser as Auth (AuthenticateUser(AuthenticateUser), AuthenticationError(AuthenticationQueryError), authenticateUser, hoist)
import qualified Infrastructure.Authentication.PasswordManager as PasswordManager
import Infrastructure.Logging.Logger (withContext, logError, logWarning)
import qualified Infrastructure.Logging.Logger as Logger
import Infrastructure.Authentication.PasswordManager (PasswordManager, PasswordManagerError(..), bcryptPasswordManager)
import Infrastructure.Persistence.PostgresContentRepository (postgresContentRepository)
import Infrastructure.Persistence.PostgresUserRepository (postgresUserRepository, UserRepositoryError (DuplicateUserName))
import qualified Tagger.ContentRepository as ContentRepository
import Tagger.ContentRepository (ContentRepository)
import qualified Tagger.UserRepository as UserRepository
import Tagger.UserRepository (UserRepository)

-- base
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Prelude hiding (log)

-- hasql
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

import qualified Infrastructure.Database as DB

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
-- Lifts a computation from 'ExceptT e IO' to 'Handler a' using the provided 'handleError' function
eitherTToHandler :: (e -> Handler a) -> ExceptT e IO a -> Handler a
eitherTToHandler handleError = either handleError pure <=< liftIO . runExceptT

-- |
-- Lifts a 'ContentRepository' fo the 'Handler' monad, handling all errors by logging them and returning a 500 response
connectedContentRepository :: Logger.Handle -> ContentRepository (ExceptT QueryError IO) -> ContentRepository Handler
connectedContentRepository logHandle = ContentRepository.hoist (eitherTToHandler $ (>> throwError err500) . logError logHandle . show)

-- |
-- Lifts a 'UserRepository' fo the 'Handler' monad, handling all errors by logging them and returning a 500 response
connectedUserRepository :: Logger.Handle -> UserRepository (ExceptT UserRepositoryError IO) -> UserRepository Handler
connectedUserRepository logHandle = UserRepository.hoist $ eitherTToHandler handleUserRepositoryError
  where
    handleUserRepositoryError :: UserRepositoryError -> Handler a
    -- If the database error concerns a duplicate user, we return a 403 response
    handleUserRepositoryError (DuplicateUserName e) = do
      logWarning logHandle $ show (DuplicateUserName e)
      throwError err403
    -- Otherwise, we return a 500 response
    handleUserRepositoryError e                     = do
      logError logHandle (show e)
      throwError err500

-- |
-- Creates an 'AuthenticateUser' service injecting its dependencies and handling errors
connectedAuthenticateUser :: Logger.Handle -> UserRepository (ExceptT UserRepositoryError IO) -> PasswordManager Handler -> Auth.AuthenticateUser Handler
connectedAuthenticateUser logHandle userRepository' passwordManager'
  = Auth.hoist (eitherTToHandler handleAuthenticationError)
  . Auth.AuthenticateUser
  $ Auth.authenticateUser userRepository' passwordManager'
    where
      handleAuthenticationError :: Auth.AuthenticationError -> Handler a
      -- If there was an error at the database level, we return a 500 response
      handleAuthenticationError (Auth.AuthenticationQueryError e) = do
        logError logHandle $ show (Auth.AuthenticationQueryError e)
        throwError err500
        -- In other cases, there was an authentication error and we return a 401 response
      handleAuthenticationError e = do
        logWarning logHandle (show e)
        throwError err401

-- |
-- Creates a 'PasswordManager' service injecting its dependencies and handling errors
encryptedPasswordManager :: Logger.Handle -> JWTSettings -> PasswordManager Handler
encryptedPasswordManager logHandle = PasswordManager.hoist (eitherTToHandler handlePasswordManagerError) . bcryptPasswordManager
  where
    handlePasswordManagerError :: PasswordManagerError -> Handler a
    -- If there was a failure during password hashing, we return a 500 response
    handlePasswordManagerError FailedHashing = do
      logError logHandle $ show FailedHashing
      throwError err500
    -- In other cases, we return a 401 response
    handlePasswordManagerError (FailedJWTCreation e) = do
      logError logHandle $ show (FailedJWTCreation e)
      throwError err401

-- |
-- Creates all the services needed by the application, creating different contexts for the logger of each service
start :: DB.Handle -> Logger.Handle -> JWK -> AppServices
start dbHandle logHandle key =
  let
    logContext = flip withContext logHandle
    passwordManager' = encryptedPasswordManager (withContext "PasswordManager" logHandle) $ defaultJWTSettings key
    dbUserRepository = postgresUserRepository dbHandle
  in  AppServices
    { jwtSettings       = defaultJWTSettings key
    , passwordManager   = passwordManager'
    , contentRepository = connectedContentRepository (logContext "ContentRepository") (postgresContentRepository dbHandle)
    , userRepository    = connectedUserRepository (logContext "UserRepository") dbUserRepository
    , authenticateUser  = connectedAuthenticateUser (logContext "AuthenticateUser") dbUserRepository passwordManager'
    }
