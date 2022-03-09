{-# LANGUAGE OverloadedStrings #-}

module TestServices where

import Api.AppServices (AppServices(..), connectedAuthenticateUser, connectedContentRepository, connectedUserRepository, encryptedPasswordManager)
import Infrastructure.Logging.Logger (provideContext, messageLogger)
import InMemoryContentRepository (inMemoryContentRepository)
import InMemoryUserRepository (inMemoryUserRepository)

-- base
import GHC.Conc (atomically, newTVar)

-- servant-auth-server
import Servant.Auth.Server (generateKey, defaultJWTSettings)

testServices :: IO AppServices
testServices = do
  key         <- generateKey
  userMap     <- atomically $ newTVar mempty
  contentsMap <- atomically $ newTVar mempty
  let passwordManager'   = encryptedPasswordManager (provideContext "PasswordManager" messageLogger) $ defaultJWTSettings key
  let userRepository'    = inMemoryUserRepository userMap
  let contentsRepository = inMemoryContentRepository contentsMap
  pure $ AppServices
    { jwtSettings       = defaultJWTSettings key
    , passwordManager   = passwordManager'
    , contentRepository = connectedContentRepository (provideContext "ContentRepository" messageLogger) contentsRepository
    , userRepository    = connectedUserRepository (provideContext "UserRepository" messageLogger) userRepository'
    , authenticateUser  = connectedAuthenticateUser (provideContext "AuthenticateUser" messageLogger) userRepository' passwordManager'
    }
