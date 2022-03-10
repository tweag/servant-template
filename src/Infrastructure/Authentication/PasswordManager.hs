{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

module Infrastructure.Authentication.PasswordManager where

import Infrastructure.Authentication.Login (Login(password), Password(asBytestring))
import Infrastructure.Authentication.Token (Token(Token))
import qualified Tagger.EncryptedPassword as Encrypted (validatePassword)
import Tagger.EncryptedPassword (EncryptedPassword, encryptPassword)
import Tagger.User (User(_password))
import Tagger.Id (Id)

-- base
import Data.Bifunctor (bimap)

-- jose
import Crypto.JWT (Error)

-- servant-auth-server
import Servant.Auth.Server (JWTSettings, makeJWT)

-- transformers
import Control.Monad.Trans.Except (ExceptT(ExceptT))

data PasswordManager m = PasswordManager
  { generatePassword :: Login -> m EncryptedPassword
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

bcryptGeneratePassword :: Login -> ExceptT PasswordManagerError IO EncryptedPassword
bcryptGeneratePassword
  = ExceptT
  . fmap (maybe (Left FailedHashing) Right)
  . encryptPassword
  . asBytestring
  . password

bcryptGenerateToken :: JWTSettings -> Id User -> ExceptT PasswordManagerError IO Token
bcryptGenerateToken jwtSettings userId = ExceptT . fmap (bimap FailedJWTCreation Token) $ makeJWT userId jwtSettings Nothing

bcryptValidatePassword :: User -> Password -> Bool
bcryptValidatePassword user password' = Encrypted.validatePassword (_password $ user) (asBytestring password')
