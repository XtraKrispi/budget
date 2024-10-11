{-# LANGUAGE DuplicateRecordFields #-}

module Web.Scotty.ActionT where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (find, traverse_)
import Data.Function (on)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Text.Lazy qualified as LT
import Effectful
import Handlers.Model
import Lucid (Html, renderText)
import Web.Scotty (ActionM)
import Web.Scotty.Cookie (SetCookie (setCookieHttpOnly, setCookieMaxAge, setCookieName, setCookieSecure, setCookieValue), defaultSetCookie, getCookies, setCookie)
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (Parsable (parseParam), html, redirect, setHeader)
import Web.Scotty.Trans qualified

captureParamMaybe :: (Monad m, Parsable a) => LT.Text -> ActionT m (Maybe a)
captureParamMaybe param = do
  params <- Web.Scotty.Trans.captureParams
  case find (\(p, _) -> p == param) params of
    Just (_, t) ->
      case parseParam t of
        Right a -> pure (Just a)
        Left _ -> pure Nothing
    Nothing -> pure Nothing

-- Toggles either send  "on" or nothing at all
toggleFormParam :: (Monad m) => LT.Text -> ActionT m Bool
toggleFormParam param = do
  params <- Web.Scotty.Trans.formParams
  case find (\(p, _) -> p == param) params of
    Just (_, "on") -> pure True
    _ -> pure False

-- Optionals either send the value or empty string
optionalFormParam :: (Monad m, Parsable a) => LT.Text -> ActionT m (Maybe a)
optionalFormParam param = do
  params <- Web.Scotty.Trans.formParams
  case find (\(p, _) -> p == param) params of
    Just (_, "") -> pure Nothing
    Just (_, txt) -> case parseParam txt of
      Right parsed -> pure (Just parsed)
      Left _ -> pure Nothing
    _ -> pure Nothing

renderHtml :: (MonadIO m) => Html a -> ActionT m ()
renderHtml = html . renderText

runHandler :: (Eff es Response -> IO Response) -> (Request -> Eff es Response) -> ActionM ()
runHandler runProgram handler = do
  headers <- fmap (bimap toStrict toStrict) <$> Web.Scotty.Trans.headers
  urlParams <- fmap (bimap toStrict toStrict) <$> Web.Scotty.Trans.captureParams
  formParams <- fmap (bimap toStrict toStrict) <$> Web.Scotty.Trans.formParams
  queryParams <- fmap (bimap toStrict toStrict) <$> Web.Scotty.Trans.queryParams

  reqCookies <- getCookies

  response <- liftIO $ runProgram $ handler $ Request reqCookies headers (formParams ++ queryParams ++ urlParams)
  case response of
    SamePage (SamePageResponse hs cookies content) -> do
      traverse_ (uncurry (setHeader `on` fromStrict)) hs
      traverse_ (setCookie . convertToCookie) cookies
      renderHtml content
    Redirect (RedirectResponse url) ->
      redirect (fromStrict url)
 where
  convertToCookie :: Cookie -> SetCookie
  convertToCookie cookie =
    defaultSetCookie
      { setCookieName = encodeUtf8 cookie.cookieName
      , setCookieValue = encodeUtf8 cookie.cookieValue
      , setCookieMaxAge = cookie.cookieMaxAge
      , setCookieHttpOnly = cookie.cookieHttpOnly
      , setCookieSecure = cookie.cookieSecure
      }