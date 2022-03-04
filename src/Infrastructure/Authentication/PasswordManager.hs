{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

module Infrastructure.Authentication.PasswordManager where

import Infrastructure.Authentication.Login (Login(password))
import Infrastructure.Authentication.Token (Token(Token))
import Tagger.User (Password(Password, asBytestring), User)

-- base
import Data.Bifunctor (bimap)

-- bcrypt
import Crypto.BCrypt (hashPasswordUsingPolicy, fastBcryptHashingPolicy)

-- jose
import Crypto.JWT (Error)

-- servant-auth-server
import Servant.Auth.Server (JWTSettings, makeJWT)

-- transformers
import Control.Monad.Trans.Except (ExceptT(ExceptT))

data PasswordManager m = PasswordManager
  { generatePassword :: Login -> m Password
  , generateToken    :: User -> m Token
  }

hoistPasswordManager :: (forall a. m a -> n a) -> PasswordManager m -> PasswordManager n
hoistPasswordManager f (PasswordManager generate verify) = PasswordManager (f . generate) (f . verify)

data PasswordManagerError
  = FailedHashing
  | FailedJWTCreation Error
  deriving stock Show

bcryptPasswordManager :: JWTSettings -> PasswordManager (ExceptT PasswordManagerError IO)
bcryptPasswordManager jwtSettings = PasswordManager
  { generatePassword = bcryptGeneratePassword
  , generateToken    = bcryptGenerateToken jwtSettings
  }

bcryptGeneratePassword :: Login -> ExceptT PasswordManagerError IO Password
bcryptGeneratePassword
  = ExceptT
  . fmap (maybe (Left FailedHashing) (Right . Password))
  . hashPasswordUsingPolicy fastBcryptHashingPolicy
  . asBytestring
  . password

bcryptGenerateToken :: JWTSettings -> User -> ExceptT PasswordManagerError IO Token
bcryptGenerateToken jwtSettings user = ExceptT . fmap (bimap FailedJWTCreation Token) $ makeJWT user jwtSettings Nothing
