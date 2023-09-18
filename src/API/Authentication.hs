module API.Authentication (API (..), api) where

import GHC.Generics (Generic)
import Infrastructure.Authentication.PasswordManager (PasswordManager (generatePassword, generateToken))
import Infrastructure.Authentication.Token (Token)
import Servant (Handler, JSON, Post, ReqBody, type (:>))
import Servant.API.Generic (type (:-))
import Servant.Server.Generic (AsServer)
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
    -- | Given some 'Login' data, generates an authentication token
    login :: mode :- "login" :> ReqBody '[JSON] Credentials :> Post '[JSON] Token
  }
  deriving stock (Generic)

api :: PasswordManager Handler -> Authenticator Handler -> UserRepository Handler -> API AsServer
api passwordManager authHandler userRepository =
  API
    { register = registerEndpoint passwordManager userRepository,
      login = loginEndpoint passwordManager authHandler
    }

registerEndpoint :: PasswordManager Handler -> UserRepository Handler -> Credentials -> Handler (Id User)
registerEndpoint passwordManager userRepository login' = do
  -- hash the password
  hashedPassword <- generatePassword passwordManager login'
  -- store the new user into the database
  UserRepository.add userRepository (username login') hashedPassword

loginEndpoint :: PasswordManager Handler -> Authenticator Handler -> Credentials -> Handler Token
loginEndpoint passwordManager authHandler login' = do
  -- try to authenticate the user
  user <- Authenticator.authUser authHandler login'
  -- if the user authenticated, generate an authentication token
  generateToken passwordManager user
