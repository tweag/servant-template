module Main where

import Api.Application (app)

-- warp
import Network.Wai.Handler.Warp (run)

main:: IO ()
main = run 8080 app
