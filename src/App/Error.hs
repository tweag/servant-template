module App.Error where

import Authentication.Error (Error (..))
import Authentication.Error qualified as Auth
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import DB.Queries (WrongNumberOfResults (..))
import DB.Repository.User.Error (UserRepositoryError (..))
import Hasql.Session (QueryError)
import Infrastructure.Authentication.PasswordManager.Error (PasswordManagerError (..))
import Infrastructure.Logger (logError, logWarning)
import Infrastructure.Logger qualified as Logger
import Servant (Handler, err401, err403, err500)

data AppError
  = QueryErr QueryError
  | UserRepositoryErr UserRepositoryError
  | PasswordManagerErr PasswordManagerError
  | AuthenticatorErr Auth.Error
  | Unauthorized
  deriving (Show)

handleAppError :: Logger.Handle -> AppError -> Handler a
handleAppError logHandle err = do
  _ <- liftIO $ print err
  case err of
    (QueryErr e) -> do
      logError logHandle . show $ e
      throwError err500
    (UserRepositoryErr (DuplicateUserName e)) -> do
      -- If the database error concerns a duplicate user, we return a 403 response
      logWarning logHandle $ show (DuplicateUserName e)
      throwError err403
    (UserRepositoryErr e) -> do
      -- Otherwise, we return a 500 response
      logError logHandle (show e)
      throwError err500
    (PasswordManagerErr FailedHashing) -> do
      -- If there was a failure during password hashing, we return a 500 response
      logError logHandle $ show FailedHashing
      throwError err500
    (PasswordManagerErr (FailedJWTCreation e)) -> do
      -- In other cases, we return a 401 response
      logError logHandle $ show (FailedJWTCreation e)
      throwError err401
    (AuthenticatorErr (AuthQueryError (UnexpectedNumberOfRows NoResults))) -> do
      -- If the user was not found, we return a 401 response
      throwError err401
    (AuthenticatorErr (AuthQueryError e)) -> do
      -- If there was an error at the database level, we return a 500 response
      logError logHandle $ show (AuthQueryError e)
      throwError err500
    (AuthenticatorErr e) -> do
      -- In other cases, there was an authentication error and we return a 401 response
      logWarning logHandle (show e)
      throwError err401
    Unauthorized -> do
      throwError err401
