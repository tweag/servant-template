module App.Services (Services (..), start) where

import App.Env
import AppM
import Authentication qualified as Auth
import DB.Repository.Content as Repo.Content
import DB.Repository.User qualified as Repo.User
import Infrastructure.Authentication.PasswordManager (PasswordManager, bcryptPasswordManager)
import Infrastructure.Authentication.PasswordManager qualified as PasswordManager
import Servant.Auth.Server (defaultJWTSettings)
import Tagger.Authentication.Authenticator qualified as Auth
import Tagger.Repository.Content (ContentRepository)
import Tagger.Repository.Content qualified as ContentRepository
import Tagger.Repository.User (UserRepository)
import Tagger.Repository.User qualified as UserRepository

-- |
-- Collection of services needed by the application to work
data Services m = Services
  { passwordManager :: PasswordManager m,
    contentRepository :: ContentRepository m,
    userRepository :: UserRepository m,
    authenticateUser :: Auth.Authenticator m
  }

start :: Env -> Services AppM
start env =
  let passwordManager =
        PasswordManager.hoist
          (changeContext "PasswordManager")
          (bcryptPasswordManager (defaultJWTSettings env.jwkKey))
      contentRepository =
        ContentRepository.hoist
          (changeContext "ContentRepository")
          Repo.Content.postgres
      userRepository =
        UserRepository.hoist
          (changeContext "UserRepository")
          Repo.User.postgres
      authenticateUser =
        Auth.hoist
          (changeContext "Authenticator")
          (Auth.authenticator Repo.User.postgres passwordManager)
   in Services
        { passwordManager,
          contentRepository,
          userRepository,
          authenticateUser
        }
