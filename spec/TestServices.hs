
module TestServices where

import Api.AppServices (AppServices(..), connectedAuthenticateUser, connectedContentRepository, connectedUserRepository, encryptedPasswordManager)
import InMemoryContentRepository (inMemoryContentRepository)
import InMemoryUserRepository (inMemoryUserRepository)
import Infrastructure.Logging.Logger as Logger
import Infrastructure.SystemTime as SystemTime

-- base
import GHC.Conc (newTVarIO)

-- servant-auth-server
import Servant.Auth.Server (generateKey, defaultJWTSettings)

testServices :: IO AppServices
testServices = do
  key         <- generateKey
  userMap     <- newTVarIO mempty
  contentsMap <- newTVarIO mempty
  SystemTime.withHandle $ \timeHandle ->
    Logger.withHandle timeHandle $ \loggerHandle -> do
      let passwordManager'   = encryptedPasswordManager loggerHandle $ defaultJWTSettings key
      let userRepository'    = inMemoryUserRepository userMap
      let contentsRepository = inMemoryContentRepository contentsMap
      pure $ AppServices
        { jwtSettings       = defaultJWTSettings key
        , passwordManager   = passwordManager'
        , contentRepository = connectedContentRepository loggerHandle contentsRepository
        , userRepository    = connectedUserRepository loggerHandle userRepository'
        , authenticateUser  = connectedAuthenticateUser loggerHandle userRepository' passwordManager'
        }
