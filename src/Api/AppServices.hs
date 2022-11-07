{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Api.AppServices where

import Control.Monad ((<=<))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Crypto.JOSE.JWK (JWK)
import Hasql.Session (QueryError)
import qualified Infrastructure.Authentication.AuthenticateUser as Auth (AuthenticateUser (AuthenticateUser), AuthenticationError (AuthenticationQueryError), authenticateUser, hoist)
import Infrastructure.Authentication.PasswordManager (PasswordManager, PasswordManagerError (..), bcryptPasswordManager)
import qualified Infrastructure.Authentication.PasswordManager as PasswordManager
import qualified Infrastructure.Database as DB
import Infrastructure.Logging.Logger (logError, logWarning, withContext)
import qualified Infrastructure.Logging.Logger as Logger
import Infrastructure.Persistence.PostgresContentRepository (postgresContentRepository)
import Infrastructure.Persistence.PostgresUserRepository (UserRepositoryError (DuplicateUserName), postgresUserRepository)
import Servant (Handler, err401, err403, err500)
import Servant.Auth.Server (JWTSettings, defaultJWTSettings)
import Tagger.ContentRepository (ContentRepository)
import qualified Tagger.ContentRepository as ContentRepository
import Tagger.UserRepository (UserRepository)
import qualified Tagger.UserRepository as UserRepository
import Prelude hiding (log)

-- |
-- Collection of services needed by the application to work
data AppServices = AppServices
  { jwtSettings :: JWTSettings,
    passwordManager :: PasswordManager Handler,
    contentRepository :: ContentRepository Handler,
    userRepository :: UserRepository Handler,
    authenticateUser :: Auth.AuthenticateUser Handler
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
    handleUserRepositoryError e = do
      logError logHandle (show e)
      throwError err500

-- |
-- Creates an 'AuthenticateUser' service injecting its dependencies and handling errors
connectedAuthenticateUser :: Logger.Handle -> UserRepository (ExceptT UserRepositoryError IO) -> PasswordManager Handler -> Auth.AuthenticateUser Handler
connectedAuthenticateUser logHandle userRepository' passwordManager' =
  Auth.hoist (eitherTToHandler handleAuthenticationError)
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

start :: DB.Handle -> Logger.Handle -> JWK -> AppServices
start dbHandle logHandle key =
  let logContext = flip withContext logHandle
      passwordManager' = encryptedPasswordManager (withContext "PasswordManager" logHandle) $ defaultJWTSettings key
      dbUserRepository = postgresUserRepository dbHandle
   in AppServices
        { jwtSettings = defaultJWTSettings key,
          passwordManager = passwordManager',
          contentRepository = connectedContentRepository (logContext "ContentRepository") (postgresContentRepository dbHandle),
          userRepository = connectedUserRepository (logContext "UserRepository") dbUserRepository,
          authenticateUser = connectedAuthenticateUser (logContext "AuthenticateUser") dbUserRepository passwordManager'
        }
