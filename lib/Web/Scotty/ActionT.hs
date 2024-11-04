{-# LANGUAGE DuplicateRecordFields #-}

module Web.Scotty.ActionT where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (fromStrict, toStrict)
import Effectful
import Handlers.Model
import Htmx.Request (isBoosted, isHtmx)
import Lucid
import Web.Scotty (ActionM)
import Web.Scotty qualified
import Web.Scotty.Cookie (SetCookie (setCookieHttpOnly, setCookieMaxAge, setCookieName, setCookieSecure, setCookieValue), defaultSetCookie, getCookies, setCookie)

runHandler :: (Eff es Response -> IO Response) -> (Request -> Eff es Response) -> ActionM ()
runHandler runProgram handler = do
  headers <- fmap (bimap toStrict toStrict) <$> Web.Scotty.headers
  urlParams <- fmap (bimap toStrict toStrict) <$> Web.Scotty.captureParams
  formParams <- fmap (bimap toStrict toStrict) <$> Web.Scotty.formParams
  queryParams <- fmap (bimap toStrict toStrict) <$> Web.Scotty.queryParams

  reqCookies <- Web.Scotty.Cookie.getCookies
  let request = Request reqCookies headers (formParams ++ queryParams ++ urlParams)
  response <- liftIO $ runProgram $ handler request
  case response of
    SamePage (SamePageResponse hs cookies content) -> do
      traverse_ (uncurry (Web.Scotty.setHeader `on` fromStrict)) hs
      traverse_ (Web.Scotty.Cookie.setCookie . convertToCookie) cookies
      Web.Scotty.html $ renderText content
    Redirect (RedirectResponse url) ->
      Web.Scotty.redirect (fromStrict url)
 where
  convertToCookie :: Cookie -> Web.Scotty.Cookie.SetCookie
  convertToCookie cookie =
    Web.Scotty.Cookie.defaultSetCookie
      { setCookieName = encodeUtf8 cookie.cookieName
      , setCookieValue = encodeUtf8 cookie.cookieValue
      , setCookieMaxAge = cookie.cookieMaxAge
      , setCookieHttpOnly = cookie.cookieHttpOnly
      , setCookieSecure = cookie.cookieSecure
      }