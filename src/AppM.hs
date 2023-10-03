module AppM (AppM, AppM', runApp, runWithContext) where

import App.Env (Env (..), Handles (..))
import App.Error
import Control.Arrow ((>>>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Text (Text)
import Infrastructure.Logger (withContext)
import Optics
import Servant (Handler)

type AppM a = AppM' a

type AppM' = ExceptT AppError (ReaderT Env IO)

runApp :: forall a. Env -> AppM a -> Handler a
runApp env computation =
  runApp' computation >>= \case
    Right a -> pure a
    Left e -> handleAppError env.handles.logger e
  where
    runApp' = runExceptT >>> flip runReaderT env >>> liftIO

runWithContext :: Text -> Env -> AppM a -> Handler a
runWithContext contextName env =
  runApp $ env & #handles % #logger %~ withContext contextName
