{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Persistence.PostgresUserRepository where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE, withExceptT)
import Data.ByteString (isInfixOf)
import Data.Text (Text)
import Data.UUID.V4 (nextRandom)
import Hasql.Session (CommandError (ResultError), QueryError (QueryError), ResultError (ServerError), Session)
import qualified Infrastructure.Database as DB
import Infrastructure.Persistence.Queries (WrongNumberOfRows)
import qualified Infrastructure.Persistence.Queries as Query
import Infrastructure.Persistence.Schema (litUser, userId)
import Infrastructure.Persistence.Serializer (serializeUser, unserializeUser)
import Tagger.EncryptedPassword (EncryptedPassword)
import Tagger.Id (Id (Id))
import Tagger.User (User (User))
import Tagger.UserRepository (UserRepository (..))

-- We want to distinguish the `QueryError` coming from the violation of the "users_name_key" unique constraints
data UserRepositoryError
  = DuplicateUserName QueryError
  | IncorrectNumberOfRows WrongNumberOfRows
  | OtherError QueryError
  deriving (Show)

-- |
-- A 'UserRepository' based on PostgreSQL
postgresUserRepository :: DB.Handle -> UserRepository Ctx
postgresUserRepository handle =
  UserRepository
    { getUserByName = postgresGetUserByName handle,
      addUser = postgresAddUser handle
    }

type Ctx = ExceptT UserRepositoryError IO

postgresGetUserByName :: DB.Handle -> Text -> Ctx (Id User, User)
postgresGetUserByName handle name = do
  eitherUser <- runQuery' handle (Query.selectUserByName name)
  case eitherUser of
    Right usr -> pure (userId usr, unserializeUser usr)
    Left e -> throwE $ IncorrectNumberOfRows e

postgresAddUser :: DB.Handle -> Text -> EncryptedPassword -> Ctx (Id User)
postgresAddUser handle name password = do
  -- Generate the UUID for the user
  userId' <- liftIO nextRandom
  let query = Query.addUser . litUser $ serializeUser (Id userId') (User name password)

  -- Actually add the user to the database, differentiating the `UserRepositoryError` cases
  runQuery' handle query
  pure $ Id userId'

runQuery' :: DB.Handle -> Session a -> Ctx a
runQuery' handle query = withExceptT liftAddUserError . ExceptT $ DB.runQuery handle query

liftAddUserError :: QueryError -> UserRepositoryError
liftAddUserError queryError@(QueryError _ _ (ResultError (ServerError "23505" message _ _)))
  | "users_name_key" `isInfixOf` message = DuplicateUserName queryError
liftAddUserError queryError = OtherError queryError
