module API.Authentication (API (..), api) where

import AppM (AppM)
import GHC.Generics (Generic)
import Infrastructure.Authentication.PasswordManager (PasswordManager (generatePassword, generateToken))
import Infrastructure.Authentication.Token (Token)
import Servant (JSON, Post, ReqBody, type (:>))
import Servant.API.Generic (type (:-))
import Servant.Server.Generic (AsServerT)
import Tagger.Authentication.Authenticator (Authenticator)
import Tagger.Authentication.Authenticator qualified as Authenticator
import Tagger.Authentication.Credentials (Credentials (username))
import Tagger.Id (Id)
import Tagger.Repository.User as UserRepository
import Tagger.User (User)

-- |
-- The endpoints required to perform authentication
data API mode = API
  { -- | Given some 'Login' data, registers a new 'User'
    register :: mode :- "register" :> ReqBody '[JSON] Credentials :> Post '[JSON] (Id User),
    -- | Given valid Credentials, generates an authentication Token
    login :: mode :- "login" :> ReqBody '[JSON] Credentials :> Post '[JSON] Token
  }
  deriving stock (Generic)

api :: PasswordManager AppM -> Authenticator AppM -> UserRepository AppM -> API (AsServerT AppM)
api passwordManager authHandler userRepository =
  API
    { register = registerEndpoint passwordManager userRepository,
      login = loginEndpoint passwordManager authHandler
    }

registerEndpoint :: PasswordManager AppM -> UserRepository AppM -> Credentials -> AppM (Id User)
registerEndpoint passwordManager userRepository login' = do
  -- hash the password
  hashedPassword <- generatePassword passwordManager login'
  -- store the new user into the database
  UserRepository.add userRepository (username login') hashedPassword

loginEndpoint :: PasswordManager AppM -> Authenticator AppM -> Credentials -> AppM Token
loginEndpoint passwordManager authHandler login' = do
  -- try to authenticate the user
  userId <- Authenticator.authUser authHandler login'
  -- if the user authenticated, generate an authentication token
  passwordManager.generateToken userId
