module TaggerSpec where

import Api.Application (app)
import TestServices (testServices)

-- hspec
import Test.Hspec (Spec, around)

-- warp
import Network.Wai.Handler.Warp (Port, testWithApplication)

withTaggerApp :: (Port -> IO ()) -> IO ()
withTaggerApp = testWithApplication $ app testServices

spec :: Spec
spec = around withTaggerApp $ do
  _
