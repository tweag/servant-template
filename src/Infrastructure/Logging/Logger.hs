{-# LANGUAGE RankNTypes #-}

module Infrastructure.Logging.Logger where

-- base
import Control.Monad.IO.Class (MonadIO (liftIO))

-- co-log-core
import Colog.Core (LogAction, cmapM, logPrintStderr, (>$<))
import Colog.Core.Severity (Severity)

-- text
import Data.Text (Text, unpack)

-- time
import Data.Time.Clock (UTCTime, getCurrentTime)

data Message a = Message
  { context  :: Text
  , severity :: Severity
  , message  :: a
  }

data Timed a = Timed
  { time  :: UTCTime
  , value :: a
  }

formatTimedMessage :: Show a => Timed (Message a) -> String
formatTimedMessage (Timed time' (Message context' severity' message'))
  =  "[" <> show severity'  <> "] "
  <> "[" <> show time'      <> "] "
  <> "[" <> unpack context' <> "] "
  <> show message'

untimedMessageLogger :: (MonadIO m) => LogAction m (Timed (Message a)) -> LogAction m (Message a)
untimedMessageLogger = cmapM $ \message' -> do
  currentTime <- liftIO getCurrentTime
  pure $ Timed currentTime message'

messageLogger :: (MonadIO m, Show a) => LogAction m (Message a)
messageLogger = untimedMessageLogger $ formatTimedMessage >$< logPrintStderr

provideContext :: Text -> LogAction m (Message a) -> LogAction m (Severity, a)
provideContext context' logger = uncurry (Message context') >$< logger
