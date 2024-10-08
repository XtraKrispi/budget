module Handlers.Global where

import Effects.WebServer
import Html.Common (addToast)
import Lucid (ToHtml (..), span_)
import Model (AlertType (..))
import Relude

clearToast :: (MonadWebServer m) => m ()
clearToast = serveHtml ""

errorToast :: (MonadWebServer m) => Text -> m ()
errorToast msg = do
  setResponseHeader "HX-Reswap" "none"
  serveHtml $ addToast Error (span_ (toHtml msg))

unknownErrorToast :: (MonadWebServer m) => m ()
unknownErrorToast = errorToast "An unknown error has occurred. Please try again."