module TestServices where

import App.Env
import App.Services (Services (..))
import AppM
import Authentication qualified as Auth
import DB.Repository.Content qualified as Repo.Content
import DB.Repository.User qualified as Repo.User
import GHC.Conc (newTVarIO)
import Infrastructure.Authentication.PasswordManager (bcryptPasswordManager)
import Infrastructure.Authentication.PasswordManager qualified as PasswordManager
import Infrastructure.Logger as Logger
import Infrastructure.SystemTime as SystemTime
import Servant.Auth.Server (defaultJWTSettings, generateKey)
import Tagger.Authentication.Authenticator qualified as Auth

mkTestEnv :: IO Env
mkTestEnv = do
  key <- generateKey
  SystemTime.withHandle $ \timeHandle ->
    Logger.withHandle timeHandle $ \loggerHandle -> do
      pure
        Env
          { config = error "[TestServices.hs] Config not loaded in tests.",
            jwkKey = key,
            handles =
              Handles
                { logger = loggerHandle,
                  database = error "[TestServices.hs] Database handle not initialized in tests.",
                  systemTime = timeHandle
                }
          }

testServices :: Env -> IO (Services AppM)
testServices env = do
  userMap <- newTVarIO mempty
  contentsMap <- newTVarIO mempty
  let userRepository = Repo.User.inMemory userMap
      contentsRepository = Repo.Content.inMemory contentsMap
      passwordManager =
        PasswordManager.hoist
          (changeContext "PasswordManager")
          (bcryptPasswordManager (defaultJWTSettings env.jwkKey))
      authenticator = Auth.authenticator userRepository passwordManager
      authenticateUser =
        Auth.hoist
          (changeContext "Authenticator")
          authenticator
  pure $
    Services
      { passwordManager = passwordManager,
        contentRepository = contentsRepository,
        userRepository = userRepository,
        authenticateUser
      }
