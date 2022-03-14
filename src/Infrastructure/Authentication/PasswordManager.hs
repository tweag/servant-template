{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

module Infrastructure.Authentication.PasswordManager where

import Infrastructure.Authentication.Credentials (Credentials(password), Password(asBytestring))
import Infrastructure.Authentication.Token (Token(Token))
import qualified Tagger.EncryptedPassword as Encrypted (validatePassword)
import Tagger.EncryptedPassword (EncryptedPassword, encryptPassword)
import Tagger.User (User(_password))
import Tagger.Id (Id)

-- base
import Control.Category ((>>>))
import Data.Bifunctor (bimap)

-- jose
import Crypto.JWT (Error)

-- servant-auth-server
import Servant.Auth.Server (JWTSettings, makeJWT)

-- transformers
import Control.Monad.Trans.Except (ExceptT(ExceptT))

-- |
-- A 'PasswordManager' is the service dedicated at dealing with password and authentication tokens
-- It is indexed by a context 'm' which wraps the results.
data PasswordManager m = PasswordManager
  { generatePassword :: Credentials -> m EncryptedPassword -- ^ given some 'Credentials', tries to encrypt the password
  , generateToken    :: Id User -> m Token                 -- ^ given a 'User' 'Id', tries to generate an authentication 'Token'
  , validatePassword :: User -> Password -> Bool           -- ^ given a 'User' and a non excrypted 'Password', checks whether the password corresponds to the user's one
  }

-- |
-- Given a natural transformation between a context 'm' and a context 'n', it allows to change the context where 'PasswordManager' is operating
hoistPasswordManager :: (forall a. m a -> n a) -> PasswordManager m -> PasswordManager n
hoistPasswordManager f (PasswordManager generate verify validate) = PasswordManager (f . generate) (f . verify) validate

-- |
-- How the 'PasswordManager' operations can fail
data PasswordManagerError
  = FailedHashing           -- ^ there was an error while hashing the password
  | FailedJWTCreation Error -- ^ there was an error while generating the authentication token
  deriving stock Show

-- |
-- A 'PasswordManager' implementation based on the 'bcrypt' algorithm
bcryptPasswordManager :: JWTSettings -> PasswordManager (ExceptT PasswordManagerError IO)
bcryptPasswordManager jwtSettings = PasswordManager
  { generatePassword = bcryptGeneratePassword
  , generateToken    = bcryptGenerateToken jwtSettings
  , validatePassword = bcryptValidatePassword
  }

bcryptGeneratePassword :: Credentials -> ExceptT PasswordManagerError IO EncryptedPassword
bcryptGeneratePassword
  -- extract the password from the Login data
  =   password
  -- convert it to bytestring
  >>> asBytestring
  -- try to encrypt it
  >>> encryptPassword
  -- wrap the error message to get a PasswordManagerError
  >>> fmap (maybe (Left FailedHashing) Right)
  -- wrap everything in ExceptT
  >>> ExceptT

bcryptGenerateToken :: JWTSettings -> Id User -> ExceptT PasswordManagerError IO Token
bcryptGenerateToken jwtSettings userId = ExceptT $ do
  -- try to generate the token containing the userId
  -- the Nothing means that the token does not expire
  token <- makeJWT userId jwtSettings Nothing
  -- wrap the error to get a PasswordErrorManager and the token to get a Token
  pure $ bimap FailedJWTCreation Token token

bcryptValidatePassword :: User -> Password -> Bool
bcryptValidatePassword user password' = Encrypted.validatePassword (_password user) (asBytestring password')
