module Handlers.Global where

import Control.Monad.IO.Class (MonadIO)
import Lucid (renderText)
import Web.Scotty.Trans (ActionT, html)

clearToast :: (MonadIO m) => ActionT m ()
clearToast =
  html $ renderText ""