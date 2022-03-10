{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

module Infrastructure.Authentication.PasswordManager where

import Infrastructure.Authentication.Login (Login(password))
import Infrastructure.Authentication.Token (Token(Token))
import Tagger.User (Password(Password, asBytestring), User (_password))
import Tagger.Id (Id)

-- base
import Data.Bifunctor (bimap)

-- bcrypt
import qualified Crypto.BCrypt as BCrypt (validatePassword)
import Crypto.BCrypt (hashPasswordUsingPolicy, fastBcryptHashingPolicy)

-- jose
import Crypto.JWT (Error)

-- servant-auth-server
import Servant.Auth.Server (JWTSettings, makeJWT)

-- transformers
import Control.Monad.Trans.Except (ExceptT(ExceptT))

data PasswordManager m = PasswordManager
  { generatePassword :: Login -> m Password
  , generateToken    :: Id User -> m Token
  , validatePassword :: User -> Password -> Bool
  }

hoistPasswordManager :: (forall a. m a -> n a) -> PasswordManager m -> PasswordManager n
hoistPasswordManager f (PasswordManager generate verify validate) = PasswordManager (f . generate) (f . verify) validate

data PasswordManagerError
  = FailedHashing
  | FailedJWTCreation Error
  deriving stock Show

bcryptPasswordManager :: JWTSettings -> PasswordManager (ExceptT PasswordManagerError IO)
bcryptPasswordManager jwtSettings = PasswordManager
  { generatePassword = bcryptGeneratePassword
  , generateToken    = bcryptGenerateToken jwtSettings
  , validatePassword = bcryptValidatePassword
  }

bcryptGeneratePassword :: Login -> ExceptT PasswordManagerError IO Password
bcryptGeneratePassword
  = ExceptT
  . fmap (maybe (Left FailedHashing) (Right . Password))
  . hashPasswordUsingPolicy fastBcryptHashingPolicy
  . asBytestring
  . password

bcryptGenerateToken :: JWTSettings -> Id User -> ExceptT PasswordManagerError IO Token
bcryptGenerateToken jwtSettings userId = ExceptT . fmap (bimap FailedJWTCreation Token) $ makeJWT userId jwtSettings Nothing

bcryptValidatePassword :: User -> Password -> Bool
bcryptValidatePassword user password' = BCrypt.validatePassword (asBytestring . _password $ user) (asBytestring password')
