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

testServices :: IO AppServices
testServices = do
  key <- generateKey
  userMap <- newTVarIO mempty
  contentsMap <- newTVarIO mempty
  SystemTime.withHandle $ \timeHandle ->
    Logger.withHandle timeHandle $ \loggerHandle -> do
      let env =
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
          userRepository = Repo.User.inMemory userMap
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
          { jwtSettings = defaultJWTSettings key,
            passwordManager = passwordManager,
            contentRepository = connectedContentRepository env contentsRepository,
            userRepository = connectedUserRepository env userRepository,
            authenticateUser
          }
