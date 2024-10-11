module Handlers.Utils where

import AppError
import Data.Foldable (find)
import Data.Text (Text)
import Effectful
import Effectful.Error.Static (Error, throwError)
import Handlers.Model
import Html.Common
import Htmx.Request qualified as Htmx
import Lucid
import Model (AlertType (Error))

getParam :: (Parse a, Error AppError :> es) => Request -> Text -> Eff es a
getParam request name =
  case find (\(n, _) -> n == name) request.requestParams of
    Just (_, param) ->
      case parse param of
        Just val -> pure val
        Nothing -> throwError (BadRequest "Bad Request")
    Nothing -> throwError (BadRequest "Bad Request")

getParamMaybe :: (Parse a, Error AppError :> es) => Request -> Text -> Eff es (Maybe a)
getParamMaybe request name =
  case find (\(n, _) -> n == name) request.requestParams of
    Just (_, param) ->
      case parse param of
        Just val -> pure $ Just val
        Nothing -> throwError (BadRequest "Bad Request")
    Nothing -> pure Nothing

getToggleParam :: Request -> Text -> Bool
getToggleParam request name = case find (\(n, _) -> n == name) request.requestParams of
  Just (_, param) -> param == "on"
  Nothing -> False

getHeader :: Request -> Text -> Maybe Text
getHeader request name = snd <$> find (\(n, _) -> n == name) request.requestHeaders

errorResponse :: Text -> Response
errorResponse msg = makeResponse [("HX-Reswap", "none")] [] (addToast Error (span_ (toHtml msg)))

redirectTo :: Request -> Text -> Response
redirectTo request url = do
  let htmx = Htmx.isHtmx request
  if htmx
    then
      headerResponse "HX-Location" url
    else
      Redirect $ RedirectResponse url

htmlResponse :: Html () -> Response
htmlResponse = SamePage . SamePageResponse [] []

headerResponse :: Text -> Text -> Response
headerResponse name val = SamePage $ SamePageResponse [(name, val)] [] mempty

makeResponse :: [Header] -> [Cookie] -> Html () -> Response
makeResponse headers cookies content = SamePage $ SamePageResponse headers cookies content
