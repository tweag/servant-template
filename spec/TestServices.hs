module TestServices where

import API.AppServices (AppServices (..), connectedAuthenticateUser, connectedContentRepository, connectedUserRepository, encryptedPasswordManager)
import App.Env
import GHC.Conc (newTVarIO)
import Impl.Authentication.Authenticator qualified as Auth
import Impl.Repository.Content qualified as Repo.Content
import Impl.Repository.User qualified as Repo.User
import Infrastructure.Authentication.PasswordManager (bcryptPasswordManager)
import Infrastructure.Logging.Logger as Logger
import Infrastructure.SystemTime as SystemTime
import Optics
import Servant.Auth.Server (defaultJWTSettings, generateKey)

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

testServices :: Env -> IO AppServices
testServices env = do
  userMap <- newTVarIO mempty
  contentsMap <- newTVarIO mempty
  let userRepository = Repo.User.inMemory userMap
      contentsRepository = Repo.Content.inMemory contentsMap
      passwordManager =
        encryptedPasswordManager
          (env & #handles % #logger %~ withContext "PasswordManager")
          (bcryptPasswordManager (defaultJWTSettings env.jwkKey))
      authenticator = Auth.authenticator userRepository passwordManager
      authenticateUser =
        connectedAuthenticateUser
          (env & #handles % #logger %~ withContext "Authenticator")
          authenticator
  pure $
    AppServices
      { passwordManager = passwordManager,
        contentRepository = connectedContentRepository env contentsRepository,
        userRepository = connectedUserRepository env userRepository,
        authenticateUser
      }
