module Impl.Repository.Content (inMemory, postgres) where

import Control.Monad.Trans.Except (ExceptT)
import Hasql.Session (QueryError)
import qualified Impl.Repository.Content.InMemory as IM
import qualified Impl.Repository.Content.Postgres as PG
import qualified Infrastructure.Database as DB
import Tagger.Repository.Content (ContentRepository (..))

postgres :: DB.Handle -> ContentRepository (ExceptT QueryError IO)
postgres = PG.repository

inMemory :: IM.Table -> ContentRepository (ExceptT QueryError IO)
inMemory = IM.repository
