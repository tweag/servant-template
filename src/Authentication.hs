module Authentication (authenticator) where

import App.Error (AppError (..))
import AppM
import Authentication.Error (Error (..))
import Control.Monad.Except (catchError)
import Control.Monad.Trans.Except (throwE)
import Infrastructure.Authentication.PasswordManager (PasswordManager (validatePassword))
import Tagger.Authentication.Authenticator (Authenticator (..))
import Tagger.Authentication.Credentials (Credentials (..))
import Tagger.Id (Id)
import Tagger.Repository.User as UserRepo
import Tagger.User (User)

authenticator ::
  UserRepository AppM' ->
  PasswordManager n ->
  Authenticator AppM'
authenticator repo pm =
  Authenticator
    { authUser = authenticateUser repo pm
    }

-- |
-- Concrete implementation of 'AuthenticateUser'.
-- Depends on a 'UserRepository' and a 'PasswordManager'
authenticateUser :: UserRepository AppM' -> PasswordManager n -> Credentials -> AppM (Id User)
authenticateUser userRepository passwordManager Credentials {username, password} = do
  (userId, user) <-
    UserRepo.findByName userRepository username `catchError` \case
      (UserRepositoryErr e') -> throwE (AuthenticatorErr $ AuthQueryError e')
      e -> throwE e
  -- check whether the provided password is the correct one
  if validatePassword passwordManager user password
    then pure userId
    else throwE (AuthenticatorErr PasswordVerificationFailed)
