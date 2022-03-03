module Api.AppServices where

import Infrastructure.Authentication.AuthenticateUser (AuthenticateUser)
import Tagger.ContentRepository (ContentRepository)
import Tagger.UserRepository (UserRepository)

-- servant-auth-server
import Servant.Auth.Server (JWTSettings)

-- servant-server
import Servant (Handler)

data AppServices = AppServices
  { jwtSettings       :: JWTSettings
  , contentRepository :: ContentRepository Handler
  , userRepository    :: UserRepository Handler
  , authenticateUser  :: AuthenticateUser Handler
  }