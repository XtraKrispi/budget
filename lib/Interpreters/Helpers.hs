module Interpreters.Helpers where

import Control.Exception (IOException)
import Control.Monad.Catch
import Data.Text
import Effectful
import Effectful.Error.Static (Error, throwError)

adapt :: (Error b :> es, IOE :> es) => (Text -> b) -> IO a -> Eff es a
adapt wrapper m = liftIO m `catch` \(e :: IOException) -> throwError . wrapper $ pack $ show e