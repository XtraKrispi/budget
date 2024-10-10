module Handlers.Global where

import Data.Text (Text)
import Effectful
import Html.Common (addToast)
import Lucid (ToHtml (..), span_)
import Model (AlertType (..))
import Web.Scotty.ActionT (renderHtml)
import Web.Scotty.Trans (ActionT, setHeader)

clearToast :: (MonadIO m) => ActionT m ()
clearToast = renderHtml ""

errorToast :: (MonadIO m) => Text -> ActionT m ()
errorToast msg = do
  setHeader "HX-Reswap" "none"
  renderHtml $ addToast Error (span_ (toHtml msg))

unknownErrorToast :: (MonadIO m) => ActionT m ()
unknownErrorToast = errorToast "An unknown error has occurred. Please try again."