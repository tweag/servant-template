module Boot (boot, Handles (..)) where

import API.Config qualified as Config
import App.Env
import Infrastructure.Database qualified as DB
import Infrastructure.Logger qualified as Logger
import Infrastructure.SystemTime qualified as SystemTime

-- |
-- Starts dependencies and yields their handles for a computation.
boot :: Config.Config -> (Handles -> IO a) -> IO a
boot appConfig action =
  SystemTime.withHandle $ \systemTime ->
    Logger.withHandle systemTime $ \logger ->
      DB.withHandle appConfig $ \database ->
        action Handles {..}
