{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Infrastructure.Logging.Logger (
  messageLogger,
  provideContext,
  Timed (..),
  Message (..),
  SeverityLogger,
  Severity(..),
  (<&)
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Colog.Core ((<&), Severity(..), LogAction, cmapM, logPrintStderr, (>$<))
import Data.Text (Text, unpack)
import Data.Time.Clock (UTCTime, getCurrentTime)

-- 'LogAction' which accepts any 'Show'able data type and a 'Severity'
type SeverityLogger m = forall a. Show a => LogAction m (Severity, a)

-- A 'Message' defines the format we want to user to log errors
data Message a = Message
  { context  :: Text     -- ^ the 'context' describes where the error happened
  , severity :: Severity -- ^ the severity describes how serious the error is
  , message  :: a        -- ^ the actual payload of the message
  }

-- 'Timed' is used to add information about the time when an error happened
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

-- Allows us to provide just a 'Message' but actually log a 'Timed Message'
untimedMessageLogger :: (MonadIO m) => LogAction m (Timed (Message a)) -> LogAction m (Message a)
untimedMessageLogger = cmapM $ \message' -> do
  currentTime <- liftIO getCurrentTime
  pure $ Timed currentTime message'

-- A 'LogAction' which requires 'Message's and logs 'Timed Message's to standard error
messageLogger :: (MonadIO m, Show a) => LogAction m (Message a)
messageLogger = untimedMessageLogger $ formatTimedMessage >$< logPrintStderr

-- Allows to add at a later time a context, so that we can log directly a message with its security level
provideContext :: Text -> LogAction m (Message a) -> LogAction m (Severity, a)
provideContext context' logger = uncurry (Message context') >$< logger
