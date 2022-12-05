module InMemoryUserRepository where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.Map.Lazy (Map, assocs, filter, insert, size)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID.V4 (nextRandom)
import GHC.Conc (TVar, atomically, readTVar, writeTVar)
import Hasql.Session (CommandError (ResultError), QueryError (QueryError), ResultError (ServerError))
import Infrastructure.Persistence.PostgresUserRepository (UserRepositoryError (DuplicateUserName))
import PostgreSQL.ErrorCodes (unique_violation)
import Tagger.EncryptedPassword (EncryptedPassword)
import Tagger.Id (Id (Id))
import Tagger.User (User (..))
import Tagger.UserRepository (SelectUserError (..), UserRepository (..))
import Prelude hiding (filter)

inMemoryUserRepository :: TVar (Map (Id User) User) -> UserRepository (ExceptT UserRepositoryError IO)
inMemoryUserRepository userMap =
  UserRepository
    { getUserByName = inMemoryGetUserByName userMap,
      addUser = inMemoryAddUser userMap
    }

inMemoryGetUserByName :: TVar (Map (Id User) User) -> Text -> ExceptT UserRepositoryError IO (Either SelectUserError (Id User, User))
inMemoryGetUserByName userMap name' = liftIO . atomically $ do
  users <- readTVar userMap
  let usersWithName = filter ((== name') . name) users
  case size usersWithName of
    0 -> pure $ Left NoUser
    1 -> pure . Right . head $ assocs usersWithName
    _ -> pure $ Left MoreThanOneUser

duplicateNameError :: Text -> UserRepositoryError
duplicateNameError name' =
  DuplicateUserName $
    QueryError
      "insert user"
      []
      ( ResultError $
          ServerError
            unique_violation
            "duplicate key value violates unique constraint"
            (Just $ "Key (name)=(" <> encodeUtf8 name' <> ") already exists")
            Nothing
      )

inMemoryAddUser :: TVar (Map (Id User) User) -> Text -> EncryptedPassword -> ExceptT UserRepositoryError IO (Id User)
inMemoryAddUser userMap name' password' = do
  userId <- Id <$> liftIO nextRandom
  queryError <- liftIO . atomically $ do
    users <- readTVar userMap
    let usersWithName = filter ((== name') . name) users
    if null usersWithName
      then writeTVar userMap (insert userId (User name' password') users) >> pure Nothing
      else pure . Just $ duplicateNameError name'
  case queryError of
    Just qe -> throwError qe
    Nothing -> pure userId
