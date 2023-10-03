module App.Services
  ( Services (..),
    start,
  )
where

import App.Env
import AppM
import Authentication qualified as Auth
import DB.Repository.Content as Repo.Content
import DB.Repository.User qualified as Repo.User
import Infrastructure.Authentication.PasswordManager (PasswordManager, bcryptPasswordManager)
import Infrastructure.Authentication.PasswordManager qualified as PasswordManager
import Servant (Handler)
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

start :: Env -> Services Handler
start env =
  let passwordManager =
        PasswordManager.hoist
          (runWithContext "PasswordManager" env)
          (bcryptPasswordManager (defaultJWTSettings env.jwkKey))
      contentRepository =
        ContentRepository.hoist
          (runWithContext "ContentRepository" env)
          Repo.Content.postgres
      userRepository =
        UserRepository.hoist
          (runWithContext "UserRepository" env)
          Repo.User.postgres
      authenticateUser = do
        Auth.hoist
          (runWithContext "Authenticator" env)
          (Auth.authenticator Repo.User.postgres passwordManager)
   in Services
        { passwordManager,
          contentRepository,
          userRepository,
          authenticateUser
        }
