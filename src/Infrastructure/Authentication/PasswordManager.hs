module Infrastructure.Authentication.PasswordManager where

import App.Error (AppError (..))
import AppM
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE)
import Data.Bifunctor (bimap)
import Infrastructure.Authentication.PasswordManager.Error (PasswordManagerError (..))
import Infrastructure.Authentication.Token (Token (Token))
import Servant.Auth.Server (JWTSettings, makeJWT)
import Tagger.Authentication.Credentials (Credentials, Password (asBytestring))
import Tagger.Authentication.Credentials qualified as Credentials (password)
import Tagger.EncryptedPassword (EncryptedPassword, encryptPassword)
import Tagger.EncryptedPassword qualified as Encrypted (validatePassword)
import Tagger.Id (Id)
import Tagger.User (User (password))

-- |
-- A 'PasswordManager' is the service dedicated at dealing with password and authentication tokens
-- It is indexed by a context 'm' which wraps the results.
data PasswordManager m = PasswordManager
  { -- | given some 'Credentials', tries to encrypt the password
    generatePassword :: Credentials -> m EncryptedPassword,
    -- | given a 'User' 'Id', tries to generate an authentication 'Token'
    generateToken :: Id User -> m Token,
    -- | given a 'User' and a non excrypted 'Password', checks whether the password corresponds to the user's one
    validatePassword :: User -> Password -> Bool
  }

-- |
-- Given a natural transformation between a context 'm' and a context 'n', it allows to change the context where 'PasswordManager' is operating
hoist :: (forall a. m a -> n a) -> PasswordManager m -> PasswordManager n
hoist f PasswordManager {generatePassword, generateToken, validatePassword} =
  PasswordManager (f . generatePassword) (f . generateToken) validatePassword

-- |
-- A 'PasswordManager' implementation based on the 'bcrypt' algorithm
bcryptPasswordManager :: JWTSettings -> PasswordManager AppM'
bcryptPasswordManager jwtSettings =
  PasswordManager
    { generatePassword = bcryptGeneratePassword,
      generateToken = bcryptGenerateToken jwtSettings,
      validatePassword = bcryptValidatePassword
    }

bcryptGeneratePassword :: Credentials -> AppM EncryptedPassword
bcryptGeneratePassword credentials = do
  -- extract the password from the Credentials
  let x = asBytestring $ Credentials.password credentials

  -- convert it to bytestring
  -- try to encrypt it
  mPwd <- liftIO $ encryptPassword x
  case mPwd of
    Nothing -> throwE (PasswordManagerErr FailedHashing)
    Just pwd -> pure pwd

-- -- wrap the error message to get a PasswordManagerError
-- >>> fmap (maybe (Left (PasswordManagerErr FailedHashing)) Right)
-- -- wrap everything in ExceptT
-- >>> ExceptT

bcryptGenerateToken :: JWTSettings -> Id User -> AppM Token
bcryptGenerateToken jwtSettings userId = ExceptT $ do
  -- try to generate the token containing the userId
  -- the Nothing means that the token does not expire
  token <- liftIO $ makeJWT userId jwtSettings Nothing
  -- wrap the error to get a PasswordErrorManager and the token to get a Token
  pure $ bimap (PasswordManagerErr . FailedJWTCreation) Token token

bcryptValidatePassword :: User -> Password -> Bool
bcryptValidatePassword user password' = Encrypted.validatePassword (password user) (asBytestring password')
