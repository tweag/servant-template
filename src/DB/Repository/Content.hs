module DB.Repository.Content (inMemory, postgres) where

import AppM (AppM')
import DB.Repository.Content.InMemory qualified as IM
import DB.Repository.Content.Postgres qualified as PG
import Tagger.Repository.Content (ContentRepository (..))

postgres :: ContentRepository AppM'
postgres = PG.repository

inMemory :: IM.Table -> ContentRepository AppM'
inMemory = IM.repository
