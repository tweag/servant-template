module Impl.Repository.Content (inMemory, postgres) where

import Control.Monad.Trans.Except (ExceptT)
import Hasql.Session (QueryError)
import Impl.Repository.Content.InMemory qualified as IM
import Impl.Repository.Content.Postgres qualified as PG
import Infrastructure.Database qualified as DB
import Tagger.Repository.Content (ContentRepository (..))

postgres :: DB.Handle -> ContentRepository (ExceptT QueryError IO)
postgres = PG.repository

inMemory :: IM.Table -> ContentRepository (ExceptT QueryError IO)
inMemory = IM.repository
