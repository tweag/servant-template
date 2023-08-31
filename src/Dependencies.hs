module Dependencies (withDeps, Deps (..)) where

import API.Config qualified as Config
import Infrastructure.Database qualified as DB
import Infrastructure.Logging.Logger qualified as Logger
import Infrastructure.SystemTime qualified as SystemTime

-- |
-- Aggregates all effects needed by the app
data Deps = Deps
  { systemTimeHandler :: SystemTime.Handle,
    loggerHandle :: Logger.Handle,
    dbHandle :: DB.Handle
  }

-- |
-- Starts dependencies and calls a given effectful function with them
withDeps :: Config.Config -> (Deps -> IO a) -> IO a
withDeps appConfig f =
  SystemTime.withHandle $ \systemTimeHandler ->
    Logger.withHandle systemTimeHandler $ \loggerHandle ->
      DB.withHandle appConfig $ \dbHandle ->
        f Deps {..}
