module Web.Scotty.ActionT where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (find)
import Data.Text (Text)
import Data.Text.Lazy qualified as LT
import Htmx.Request qualified as Htmx
import Lucid (Html, renderText)
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (Parsable (parseParam), captureParams, formParams, html, redirect, setHeader)

captureParamMaybe :: (Monad m, Parsable a) => LT.Text -> ActionT m (Maybe a)
captureParamMaybe param = do
  params <- captureParams
  case find (\(p, _) -> p == param) params of
    Just (_, t) ->
      case parseParam t of
        Right a -> pure (Just a)
        Left _ -> pure Nothing
    Nothing -> pure Nothing

-- Toggles either send  "on" or nothing at all
toggleFormParam :: (Monad m) => LT.Text -> ActionT m Bool
toggleFormParam param = do
  params <- formParams
  case find (\(p, _) -> p == param) params of
    Just (_, "on") -> pure True
    _ -> pure False

-- Optionals either send the value or empty string
optionalFormParam :: (Monad m, Parsable a) => LT.Text -> ActionT m (Maybe a)
optionalFormParam param = do
  params <- formParams
  case find (\(p, _) -> p == param) params of
    Just (_, "") -> pure Nothing
    Just (_, txt) -> case parseParam txt of
      Right parsed -> pure (Just parsed)
      Left _ -> pure Nothing
    _ -> pure Nothing

redirectTo :: (MonadIO m) => Text -> ActionT m ()
redirectTo url = do
  htmx <- Htmx.isHtmx
  if htmx
    then
      setHeader "HX-Location" (LT.fromStrict url)
    else
      redirect (LT.fromStrict url)

renderHtml :: (MonadIO m) => Html a -> ActionT m ()
renderHtml = html . renderText
