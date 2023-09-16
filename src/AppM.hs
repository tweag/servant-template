module AppM (AppM, AppM', runComponent) where

import App.Env (Env (..), Handles (..))
import App.Error
import Control.Arrow ((>>>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
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

runComponent :: ((forall a. AppM a -> Handler a) -> b) -> Env -> b
runComponent hoister env = hoister (runApp env)

-- newtype NewAppM a = NewAppM {runNewAppM :: AppM' a}

-- -- |
-- -- Lifts a computation from 'NewAppM' to 'Handler a' using the provided 'handleError' function
-- runApp' :: Env -> NewAppM a -> Handler a
-- runApp' env (NewAppM computation) =
--   (liftIO . runExceptT $ computation) >>= \case
--     Right a -> pure a
--     Left e -> handleAppError env.handles.logger e
-- g :: ((NewAppM a -> Handler a) -> b) -> Env -> b
-- g hoister = hoister . runApp
