module Tagger.Database (runQuery) where

import App.Env
import AppM
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Hasql.Session (Session)
import Infrastructure.Database qualified as Infra

runQuery :: Session a -> AppM a
runQuery query = do
  handle <- asks (.handles.database)
  liftIO (Infra.runQuery handle query) >>= \case
    Left e -> throwError e
    Right a -> pure a
