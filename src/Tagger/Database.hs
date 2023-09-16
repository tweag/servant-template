module Tagger.Database (runQuery) where

import App.Env
import AppM
import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Except (throwE)
import Hasql.Session (Session)
import Infrastructure.Database qualified as Infra

runQuery :: Session a -> AppM a
runQuery query = do
  handle <- asks (.handles.database)
  liftIO (Infra.runQuery handle query) >>= \case
    Left e -> throwE e
    Right a -> pure a
