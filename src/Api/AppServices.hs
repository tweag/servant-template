{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Api.AppServices
  ( AppServices (..),
    appServices,
    connectedContentRepository,
    connectedUserRepository,
    connectedAuthenticateUser,
    encryptedPasswordManager,
  )
where

import Control.Monad ((<=<))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Crypto.JOSE.JWK (JWK)
import Hasql.Connection (Connection)
import Hasql.Session (QueryError)
import qualified Infrastructure.Authentication.AuthenticateUser as Auth
import Infrastructure.Authentication.PasswordManager
  ( PasswordManager,
    PasswordManagerError (..),
    bcryptPasswordManager,
  )
import qualified Infrastructure.Authentication.PasswordManager as PasswordManager
import Infrastructure.Logging.Logger
  ( Severity (..),
    SeverityLogger,
    messageLogger,
    provideContext,
    (<&),
  )
import Infrastructure.Persistence.PostgresContentRepository (postgresContentRepository)
import Infrastructure.Persistence.PostgresUserRepository (UserRepositoryError (DuplicateUserName), postgresUserRepository)
import Servant (Handler, err401, err403, err500)
import Servant.Auth.Server (JWTSettings, defaultJWTSettings)
import Tagger.ContentRepository (ContentRepository)
import qualified Tagger.ContentRepository as ContentRepository
import Tagger.UserRepository (UserRepository)
import qualified Tagger.UserRepository as UserRepository
import Prelude hiding (log)

-- Collection of services needed by the application to work
data AppServices = AppServices
  { jwtSettings :: JWTSettings,
    passwordManager :: PasswordManager Handler,
    contentRepository :: ContentRepository Handler,
    userRepository :: UserRepository Handler,
    authenticateUser :: Auth.AuthenticateUser Handler
  }

type Log = SeverityLogger Handler

-- Creates all the services needed by the application, creating a different contexts for the logger of each service
appServices :: Connection -> JWK -> AppServices
appServices connection key =
  let logContext name = provideContext name messageLogger
      passwordManager' = encryptedPasswordManager (logContext "PasswordManager") $ defaultJWTSettings key
      dbUserRepository = postgresUserRepository connection
   in AppServices
        { jwtSettings = defaultJWTSettings key,
          passwordManager = passwordManager',
          contentRepository = connectedContentRepository (logContext "ContentRepository") (postgresContentRepository connection),
          userRepository = connectedUserRepository (logContext "UserRepository") dbUserRepository,
          authenticateUser = connectedAuthenticateUser (logContext "AuthenticateUser") dbUserRepository passwordManager'
        }

-- Lifts a computation from 'ExceptT e IO' to 'Handler a' using the provided 'handleError' function
eitherTToHandler :: (e -> Handler a) -> ExceptT e IO a -> Handler a
eitherTToHandler handleError = either handleError pure <=< liftIO . runExceptT

-- Lifts a 'ContentRepository' to the 'Handler' monad, handling all errors by logging them and returning a 500 response
connectedContentRepository :: Log -> ContentRepository (ExceptT QueryError IO) -> ContentRepository Handler
connectedContentRepository log = ContentRepository.hoist (eitherTToHandler $ (>> throwError err500) . (log <&) . (Error,))

-- Lifts a 'UserRepository' to the 'Handler' monad, handling all errors by logging them and returning a 500 response
connectedUserRepository ::
  Log ->
  UserRepository (ExceptT UserRepositoryError IO) ->
  UserRepository Handler
connectedUserRepository log = UserRepository.hoist $ eitherTToHandler handleUserRepositoryError
  where
    handleUserRepositoryError :: UserRepositoryError -> Handler a
    handleUserRepositoryError = \case
      (DuplicateUserName e) -> do
        log <& (Warning, DuplicateUserName e)
        throwError err403
      e -> do
        log <& (Error, e)
        throwError err500

-- Creates an 'AuthenticateUser' service injecting its dependencies and handling errors
connectedAuthenticateUser :: Log -> UserRepository (ExceptT UserRepositoryError IO) -> PasswordManager Handler -> Auth.AuthenticateUser Handler
connectedAuthenticateUser log userRepository' passwordManager' =
  Auth.hoist (eitherTToHandler handleAuthenticationError)
    . Auth.AuthenticateUser
    $ Auth.authenticateUser userRepository' passwordManager'
  where
    handleAuthenticationError :: Auth.AuthenticationError -> Handler a
    handleAuthenticationError = \case
      -- If there was an error at the database level, return a 500 response
      Auth.AuthenticationQueryError e -> do
        log <& (Error, Auth.AuthenticationQueryError e)
        throwError err500

      -- Otherwise there was an authentication error, return a 401 response
      e -> do
        log <& (Warning, e)
        throwError err401

-- Creates a 'PasswordManager' service injecting its dependencies and handling errors
encryptedPasswordManager :: Log -> JWTSettings -> PasswordManager Handler
encryptedPasswordManager log = PasswordManager.hoist (eitherTToHandler handlePasswordManagerError) . bcryptPasswordManager
  where
    handlePasswordManagerError :: PasswordManagerError -> Handler a
    -- If there was a failure during password hashing, we return a 500 response
    handlePasswordManagerError FailedHashing = do
      log <& (Error, FailedHashing)
      throwError err500

    -- In other cases, return a 401 response
    handlePasswordManagerError (FailedJWTCreation e) = do
      log <& (Error, FailedJWTCreation e)
      throwError err401
