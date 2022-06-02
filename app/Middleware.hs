{-# LANGUAGE OverloadedStrings #-}

module Middleware (withMiddlewares) where

import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Network.Wai as Wai (Application, Middleware)

withMiddlewares :: Wai.Application -> Wai.Application
withMiddlewares =
 corsMiddleware . logStdoutDev

corsMiddleware :: Wai.Middleware
corsMiddleware =
  let headers = ["Authorization", "Content-Type"]
   in cors (const . Just $ simpleCorsResourcePolicy {corsRequestHeaders = headers})
