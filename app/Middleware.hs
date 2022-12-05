module Middleware (apply) where

import qualified Network.Wai as Wai (Application, Middleware)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

apply :: Wai.Application -> Wai.Application
apply =
  corsMiddleware . logStdoutDev

corsMiddleware :: Wai.Middleware
corsMiddleware =
  let headers = ["Authorization", "Content-Type"]
   in cors (const . Just $ simpleCorsResourcePolicy {corsRequestHeaders = headers})
