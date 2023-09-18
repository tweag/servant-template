module DB.Repository.User.Postgres (repository) where

import App.Error (AppError (..))
import AppM (AppM, AppM')
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import DB.Queries qualified as Query
import DB.Repository.User.Error (UserRepositoryError (..))
import DB.Schema.User (litUser, serializeUser, unserializeUser, userId)
import Data.ByteString (isInfixOf)
import Data.Text (Text)
import Data.UUID.V4 (nextRandom)
import Hasql.Session (CommandError (ResultError), QueryError (QueryError), ResultError (ServerError), Session)
import Tagger.Database (runQuery)
import Tagger.EncryptedPassword (EncryptedPassword)
import Tagger.Id (Id (Id))
import Tagger.Repository.User (UserRepository (..))
import Tagger.User (User (User))

-- |
-- A 'UserRepository' based on PostgreSQL
repository :: UserRepository AppM'
repository =
  UserRepository
    { findByName = postgresGetUserByName,
      add = postgresAddUser
    }

postgresGetUserByName :: Text -> AppM (Id User, User)
postgresGetUserByName name = do
  eitherUser <- runRepositoryQuery (Query.selectUserByName name)
  case eitherUser of
    Right usr -> pure (userId usr, unserializeUser usr)
    Left e -> throwE $ UserRepositoryErr (UnexpectedNumberOfRows e)

postgresAddUser :: Text -> EncryptedPassword -> AppM (Id User)
postgresAddUser name password = do
  -- Generate the UUID for the user
  userId' <- liftIO nextRandom
  let query = Query.addUser . litUser $ serializeUser (Id userId') (User name password)

  -- Actually add the user to the database, differentiating the `UserRepositoryError` cases
  runRepositoryQuery query
  pure $ Id userId'

-- | Run a query transforming a Hasql.QueryError into a UserRepositoryError as appropriate to the
-- domain.
runRepositoryQuery :: Session a -> AppM a
runRepositoryQuery session =
  runQuery session `catchError` (throwE . liftE)
  where
    liftE :: AppError -> AppError
    liftE (QueryErr e) = UserRepositoryErr $ liftRepositoryError e
    liftE e = e

    liftRepositoryError :: QueryError -> UserRepositoryError
    liftRepositoryError queryError@(QueryError _ _ (ResultError (ServerError "23505" message _ _ _)))
      | "users_name_key" `isInfixOf` message = DuplicateUserName queryError
    liftRepositoryError queryError = OtherError queryError
