module Infrastructure.SystemTime
  ( Handle,
    withHandle,
    currentTime,
    UTCTime,
  )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Time.Clock (UTCTime, getCurrentTime)

data Handle = Handle

-- |
-- Yields a handle
withHandle :: (Handle -> IO a) -> IO a
withHandle = bracket new close

-- |
-- Returns current time
currentTime :: MonadIO m => Handle -> m UTCTime
currentTime = const $ liftIO getCurrentTime

-- |
-- Creates new handle
new :: MonadIO m => m Handle
new = pure Handle

-- |
-- Cleanup function
close :: MonadIO m => Handle -> m ()
close = const $ pure ()
