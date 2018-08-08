module Main where

import           ClassyPrelude
import           Data.Proxy                           (Proxy (..))
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant.Server                       (Server, serve)
import           Servant.Swagger.UI.ReDoc             (SwaggerSchemaUI,
                                                       redocSchemaUIServer)

import           Conscient.Routes

--------------------------------------------------------------------------------
type API =
  SwaggerSchemaUI
  "docs"
  "resources/swagger.json"

server :: Server API
server = redocSchemaUIServer routesSpecification

api :: Proxy API
api = Proxy :: Proxy API

main :: IO ()
main = do
  let app = serve api server
  run 8080 . logStdoutDev $ app
