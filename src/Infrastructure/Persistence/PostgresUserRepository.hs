module Infrastructure.Persistence.PostgresUserRepository where

import Infrastructure.Persistence.Queries (addUser)
import Tagger.UserRepository (UserRepository(UserRepository))

-- base
import Control.Monad.IO.Class (liftIO)

-- hasql
import Hasql.Connection (Connection)
import Hasql.Session (QueryError, run)

-- transformers
import Control.Monad.Trans.Except (ExceptT(ExceptT))

-- uuid
import Data.UUID.V4 (nextRandom)
import Infrastructure.Persistence.Serializer (serializeUser)
import Infrastructure.Persistence.Schema (litUser)
import Tagger.User (Password(Password), User(User))

postgresUserRepository :: Connection -> UserRepository (ExceptT QueryError IO)
postgresUserRepository connection = UserRepository
  (\name password -> do
    userId <- liftIO nextRandom
    ExceptT $ run (addUser . litUser $ serializeUser userId (User name (Password password))) connection
    pure userId)