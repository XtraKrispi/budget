module Handlers.Global where

import Html.Common (addToast)
import Lucid (ToHtml (..), renderText, span_)
import Model (AlertType (..))
import Relude
import Web.Scotty.Trans (ActionT, html, setHeader)

clearToast :: (MonadIO m) => ActionT m ()
clearToast =
  html $ renderText ""

errorToast :: (MonadIO m) => Text -> ActionT m ()
errorToast msg = do
  setHeader "HX-Reswap" "none"
  html $ renderText $ addToast Error (span_ (toHtml msg))