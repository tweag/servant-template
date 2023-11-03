module AppM (AppM, runApp, changeContext) where

import App.Env
import App.Error
import Control.Arrow ((>>>))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (local))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Text (Text)
import Infrastructure.Logger (withContext)
import Optics

-- | The application monad, providing access to the environment and error handling
newtype AppM a = AppM {runAppM :: ExceptT AppError (ReaderT Env IO) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Env,
      MonadError AppError
    )

-- | Run the application, providing the environment and returning the result
runApp :: (MonadIO m) => forall a. Env -> AppM a -> m (Either AppError a)
runApp env = runAppM >>> runExceptT >>> flip runReaderT env >>> liftIO

-- | Change the context of the logger for the duration of the computation.
changeContext :: Text -> AppM a -> AppM a
changeContext contextName computation =
  local (\env -> env & #handles % #logger %~ withContext contextName) $ do
    computation
