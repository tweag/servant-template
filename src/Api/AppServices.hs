module Api.AppServices where

import Infrastructure.Authentication.AuthenticateUser (AuthenticateUser)
import Infrastructure.Authentication.PasswordManager (PasswordManager)
import Tagger.ContentRepository (ContentRepository)
import Tagger.UserRepository (UserRepository)

-- servant-auth-server
import Servant.Auth.Server (JWTSettings)

-- servant-server
import Servant (Handler)

data AppServices = AppServices
  { jwtSettings       :: JWTSettings
  , passwordManager   :: PasswordManager Handler
  , contentRepository :: ContentRepository Handler
  , userRepository    :: UserRepository Handler
  , authenticateUser  :: AuthenticateUser Handler
  }
