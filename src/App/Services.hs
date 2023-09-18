module App.Services
  ( Services (..),
    start,
    connectedContentRepository,
    connectedUserRepository,
    connectedAuthenticateUser,
    encryptedPasswordManager,
  )
where

import App.Env
import AppM
import Impl.Authentication.Authenticator qualified as Auth
import DB.Repository.Content as Repo.Content
import DB.Repository.User qualified as Repo.User
import Infrastructure.Authentication.PasswordManager (PasswordManager, bcryptPasswordManager)
import Infrastructure.Authentication.PasswordManager qualified as PasswordManager
import Infrastructure.Logger (withContext)
import Optics
import Servant (Handler)
import Servant.Auth.Server (defaultJWTSettings)
import Tagger.Authentication.Authenticator (Authenticator)
import Tagger.Authentication.Authenticator qualified as Auth
import Tagger.Repository.Content (ContentRepository)
import Tagger.Repository.Content qualified as ContentRepository
import Tagger.Repository.User (UserRepository)
import Tagger.Repository.User qualified as UserRepository
import Prelude hiding (log)

-- |
-- Collection of services needed by the application to work
data Services = Services
  { passwordManager :: PasswordManager Handler,
    contentRepository :: ContentRepository Handler,
    userRepository :: UserRepository Handler,
    authenticateUser :: Auth.Authenticator Handler
  }

start :: Env -> Services
start env =
  let passwordManager =
        encryptedPasswordManager
          (env & #handles % #logger %~ withContext "PasswordManager")
          (bcryptPasswordManager (defaultJWTSettings env.jwkKey))
      dbUserRepository = Repo.User.postgres
      authenticator = Auth.authenticator dbUserRepository passwordManager
      contentRepository =
        connectedContentRepository
          (env & #handles % #logger %~ withContext "ContentRepository")
          Repo.Content.postgres
      userRepository =
        connectedUserRepository
          (env & #handles % #logger %~ withContext "UserRepository")
          dbUserRepository
      authenticateUser =
        connectedAuthenticateUser
          (env & #handles % #logger %~ withContext "Authenticator")
          authenticator
   in Services
        { passwordManager,
          contentRepository,
          userRepository,
          authenticateUser
        }

-- |
-- Creates a 'PasswordManager' service injecting its dependencies and handling errors
encryptedPasswordManager :: Env -> PasswordManager AppM' -> PasswordManager Handler
encryptedPasswordManager = runComponent PasswordManager.hoist

-- |
-- Lifts a 'ContentRepository' fo the 'Handler' monad, handling all errors by logging them and returning a 500 response
connectedContentRepository :: Env -> ContentRepository AppM' -> ContentRepository Handler
connectedContentRepository = runComponent ContentRepository.hoist

-- |
-- Lifts a 'UserRepository' fo the 'Handler' monad, handling all errors by logging them and returning a 500 response
connectedUserRepository :: Env -> UserRepository AppM' -> UserRepository Handler
connectedUserRepository = runComponent UserRepository.hoist

-- |
-- Creates an 'AuthenticateUser' service injecting its dependencies and handling errors
connectedAuthenticateUser :: Env -> Authenticator AppM' -> Auth.Authenticator Handler
connectedAuthenticateUser = runComponent Auth.hoist
