module Interpreters.WebServer where

import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effects.WebServer (WebServer (..))
import Network.Wai.Handler.Warp (run)

runWebServerWarp :: (IOE :> es) => Eff (WebServer : es) a -> Eff es a
runWebServerWarp = interpret \_ -> \case
  Serve port app -> liftIO $ do
    putStrLn $ "Starting web server on port " <> show port
    run port app