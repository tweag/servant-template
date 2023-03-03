module TestServices where

import API.AppServices (AppServices (..), connectedAuthenticateUser, connectedContentRepository, connectedUserRepository, encryptedPasswordManager)
import GHC.Conc (newTVarIO)
import qualified Impl.Repository.Content as Repo.Content
import qualified Impl.Repository.User as Repo.User
import Infrastructure.Logging.Logger as Logger
import Infrastructure.SystemTime as SystemTime
import Servant.Auth.Server (defaultJWTSettings, generateKey)

testServices :: IO AppServices
testServices = do
  key <- generateKey
  userMap <- newTVarIO mempty
  contentsMap <- newTVarIO mempty
  SystemTime.withHandle $ \timeHandle ->
    Logger.withHandle timeHandle $ \loggerHandle -> do
      let passwordManager' = encryptedPasswordManager loggerHandle $ defaultJWTSettings key
      let userRepository' = Repo.User.inMemory userMap
      let contentsRepository = Repo.Content.inMemory contentsMap
      pure $
        AppServices
          { jwtSettings = defaultJWTSettings key,
            passwordManager = passwordManager',
            contentRepository = connectedContentRepository loggerHandle contentsRepository,
            userRepository = connectedUserRepository loggerHandle userRepository',
            authenticateUser = connectedAuthenticateUser loggerHandle userRepository' passwordManager'
          }
