{-# LANGUAGE OverloadedStrings #-}

module TestServices where

import Api.AppServices (AppServices(..), connectedAuthenticateUser, connectedContentRepository, connectedUserRepository, encryptedPasswordManager)
import InMemoryContentRepository (inMemoryContentRepository)
import InMemoryUserRepository (inMemoryUserRepository)

-- base
import GHC.Conc (newTVarIO)

-- servant-auth-server
import Servant.Auth.Server (generateKey, defaultJWTSettings)

testServices :: IO AppServices
testServices = do
  key         <- generateKey
  userMap     <- newTVarIO mempty
  contentsMap <- newTVarIO mempty
  let passwordManager'   = encryptedPasswordManager mempty $ defaultJWTSettings key
  let userRepository'    = inMemoryUserRepository userMap
  let contentsRepository = inMemoryContentRepository contentsMap
  pure $ AppServices
    { jwtSettings       = defaultJWTSettings key
    , passwordManager   = passwordManager'
    , contentRepository = connectedContentRepository mempty contentsRepository
    , userRepository    = connectedUserRepository mempty userRepository'
    , authenticateUser  = connectedAuthenticateUser mempty userRepository' passwordManager'
    }
