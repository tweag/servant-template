module Dependencies (start, AppDependencies (..)) where

import qualified Api.Config as Config
import qualified Infrastructure.Database as DB
import qualified Infrastructure.Logging.Logger as Logger
import qualified Infrastructure.SystemTime as SystemTime

-- |
-- Aggregates all effects needed by the app
data AppDependencies = Dependencies
  { systemTime :: SystemTime.Handle,
    logger :: Logger.Handle,
    database :: DB.Handle
  }

-- |
-- Acquires handles for all dependencies and stores them in
-- the AppDependencies data structure for later use by the app
start :: Config.Config -> IO AppDependencies
start appConfig = do
  SystemTime.withHandle $ \systemTimeHandle ->
    Logger.withHandle systemTimeHandle $ \loggerHandle ->
      DB.withHandle appConfig $ \dbHandle -> do
        pure $ Dependencies systemTimeHandle loggerHandle dbHandle
