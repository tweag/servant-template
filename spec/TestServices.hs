{-# LANGUAGE OverloadedStrings #-}

module TestServices where

import Api.AppServices (AppServices(..), connectedAuthenticateUser, connectedContentRepository, connectedUserRepository, encryptedPasswordManager)
import Infrastructure.Logging.Logger (provideContext, messageLogger)
import InMemoryUserRepository (inMemoryUserRepository)
import Tagger.UserRepository (UserRepository)

-- base
import GHC.Conc (atomically, newTVar)

-- hasql
import Hasql.Session (QueryError)

-- servant-auth-server
import Servant.Auth.Server (generateKey, defaultJWTSettings)

-- transformers
import Control.Monad.Trans.Except (ExceptT)

testServices :: IO AppServices
testServices = do
  key <- generateKey
  userMap <- atomically $ newTVar mempty
  let passwordManager = encryptedPasswordManager (provideContext "PasswordManager" messageLogger) $ defaultJWTSettings key
  let userRepository  = inMemoryUserRepository userMap
  pure $ AppServices
    { jwtSettings       = defaultJWTSettings key
    , passwordManager   = passwordManager
    , contentRepository = connectedContentRepository (provideContext "ContentRepository" messageLogger) _
    , userRepository    = connectedUserRepository (provideContext "UserRepository" messageLogger) userRepository
    , authenticateUser  = connectedAuthenticateUser (provideContext "AuthenticateUser" messageLogger) userRepository passwordManager
    }
