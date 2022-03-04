module Api.AppServices where

import qualified Infrastructure.Authentication.AuthenticateUser as Auth (AuthenticateUser(AuthenticateUser), AuthenticationError(AuthenticationQueryError), authenticateUser, hoistAuthenticateUser)
import Infrastructure.Authentication.PasswordManager (PasswordManager, PasswordManagerError(..), hoistPasswordManager, bcryptPasswordManager)
import Infrastructure.Persistence.PostgresContentRepository (postgresContentRepository)
import Infrastructure.Persistence.PostgresUserRepository (postgresUserRepository)
import Tagger.ContentRepository (ContentRepository, hoistContentRepository)
import Tagger.UserRepository (UserRepository, hoistUserRepository)

-- base
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)

-- hasql
import Hasql.Connection (Connection)

-- jose
import Crypto.JOSE.JWK (JWK)

-- mtl
import Control.Monad.Except (throwError)

-- servant-auth-server
import Servant.Auth.Server (JWTSettings, defaultJWTSettings)

-- servant-server
import Servant (Handler, err500, err401)

-- transformers
import Control.Monad.Trans.Except (ExceptT, runExceptT)

data AppServices = AppServices
  { jwtSettings       :: JWTSettings
  , passwordManager   :: PasswordManager Handler
  , contentRepository :: ContentRepository Handler
  , userRepository    :: UserRepository Handler
  , authenticateUser  :: Auth.AuthenticateUser Handler
  }

eitherTToHandler :: (e -> Handler a) -> ExceptT e IO a -> Handler a
eitherTToHandler handleError = either handleError pure <=< liftIO . runExceptT

connectedContentRepository :: Connection -> ContentRepository Handler
connectedContentRepository = hoistContentRepository (eitherTToHandler . const $ throwError err500) . postgresContentRepository

connectedUserRepository :: Connection -> UserRepository Handler
connectedUserRepository = hoistUserRepository (eitherTToHandler . const $ throwError err500). postgresUserRepository

connectedAuthenticateUser :: Connection -> Auth.AuthenticateUser Handler
connectedAuthenticateUser = Auth.hoistAuthenticateUser (eitherTToHandler handleAuthenticationError) . Auth.AuthenticateUser . Auth.authenticateUser
  where
    handleAuthenticationError :: Auth.AuthenticationError -> Handler a
    handleAuthenticationError (Auth.AuthenticationQueryError _) = throwError err500
    handleAuthenticationError _                                 = throwError err401

encryptedPasswordManager :: JWTSettings -> PasswordManager Handler
encryptedPasswordManager = hoistPasswordManager (eitherTToHandler handlePasswordManagerError) . bcryptPasswordManager
  where
    handlePasswordManagerError :: PasswordManagerError -> Handler a
    handlePasswordManagerError FailedHashing         = throwError err500
    handlePasswordManagerError (FailedJWTCreation _) = throwError err401

appServices :: Connection -> JWK -> AppServices
appServices connection key = AppServices
  { jwtSettings       = defaultJWTSettings key
  , passwordManager   = encryptedPasswordManager $ defaultJWTSettings key
  , contentRepository = connectedContentRepository connection
  , userRepository    = connectedUserRepository connection
  , authenticateUser  = connectedAuthenticateUser connection
  }
