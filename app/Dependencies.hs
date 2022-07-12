module Dependencies (withDeps, Deps (..)) where

import qualified Api.Config as Config
import qualified Infrastructure.Database as DB
import qualified Infrastructure.Logging.Logger as Logger
import qualified Infrastructure.SystemTime as SystemTime

-- |
-- Aggregates all effects needed by the app
data Deps = Deps
  { systemTimeHandler :: SystemTime.Handle,
    loggerHandle :: Logger.Handle,
    dbHandle :: DB.Handle
  }

-- |
-- Starts dependencies and calls a given effectful function with them
withDeps :: Config.Config -> (Deps  -> IO a) -> IO a
withDeps appConfig f = do
  SystemTime.withHandle $ \systemTime ->
    Logger.withHandle systemTime $ \logger ->
      DB.withHandle appConfig $ \db -> do
        f $ Deps systemTime logger db
