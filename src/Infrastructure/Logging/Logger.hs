module Infrastructure.Logging.Logger
  ( Handle,
    withHandle,
    withContext,
    logError,
    logInfo,
    logWarning,
    logDebug,
  )
where

import Colog.Core (Severity (..), logStringStderr, logStringStdout, (<&))
import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Infrastructure.SystemTime (UTCTime)
import qualified Infrastructure.SystemTime as SystemTime
import Prelude hiding (log)

newtype Config = Config
  { logLevel :: Severity
  }

data Handle = Handle
  { systemTimeHandle :: SystemTime.Handle,
    localContext :: Maybe Context,
    minLevel :: Severity
  }

type Context = Text

-- |
-- Uses dependencies to yield a handle
withHandle :: SystemTime.Handle -> (Handle -> IO a) -> IO a
withHandle timeHandle f = do
  bracket
    (new parseConfig timeHandle)
    close
    f

-- |
-- Returns new handle that logs within a specific context.
withContext :: Context -> Handle -> Handle
withContext context handle = handle {localContext = Just context}

-- |
-- Logs message with severity set to Error
logError :: MonadIO m => Handle -> String -> m ()
logError = log Error

-- |
-- Logs message with severity set to Info
logInfo :: MonadIO m => Handle -> String -> m ()
logInfo = log Info

-- |
-- Logs message with severity set to Warning
logWarning :: MonadIO m => Handle -> String -> m ()
logWarning = log Warning

-- |
-- Logs message with severity set to Debug
logDebug :: MonadIO m => Handle -> String -> m ()
logDebug = log Debug

-- |
-- Logs timestamped message with added severity and context
-- information to appropriate file descriptor if severity meets
-- minimum configured level
log :: (MonadIO m) => Severity -> Handle -> String -> m ()
log level handle msg = do
  currentTime <- SystemTime.currentTime $ systemTimeHandle handle
  let formattedLine = format currentTime level (localContext handle) msg
  when (level >= minLevel handle) (logAction <& formattedLine)
  where
    logAction =
      case level of
        Error ->
          logStringStderr
        _ ->
          logStringStdout

-- |
-- Creates new handle
new :: Config -> SystemTime.Handle -> IO Handle
new config timeHandle = do
  pure $
    Handle
      { systemTimeHandle = timeHandle,
        localContext = Nothing,
        minLevel = logLevel config
      }

-- |
-- Cleanup function
close :: Handle -> IO ()
close = const $ pure ()

-- |
-- Create Logger config
parseConfig :: Config
parseConfig = Config Info

newtype Unquoted = Unquoted String

instance Show Unquoted where
  show (Unquoted str) = str

-- |
-- Formats in following format:
-- [Severity] [2022-06-13 14:54:39.043078872 UTC] [Context] Log message
-- or (without context):
-- [Severity] [2022-06-13 14:54:39.043078872 UTC] Log message
format :: UTCTime -> Severity -> Maybe Context -> String -> String
format time severity ctx msg =
  withBrackets severity <> withBrackets time <> maybe "" withBrackets ctx <> msg
  where
    withBrackets s =
      "[" <> show s <> "] "
