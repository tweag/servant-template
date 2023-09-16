module Impl.Repository.Content (inMemory, postgres) where

import AppM (AppM')
import Impl.Repository.Content.InMemory qualified as IM
import Impl.Repository.Content.Postgres qualified as PG
import Tagger.Repository.Content (ContentRepository (..))

postgres :: ContentRepository AppM'
postgres = PG.repository

inMemory :: IM.Table -> ContentRepository AppM'
inMemory = IM.repository
