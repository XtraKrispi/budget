module Handlers.Login where

import Data.Time (
  addUTCTime,
  secondsToNominalDiffTime,
 )
import Effects.Session
import Effects.Time
import Effects.User (MonadUser (..))
import Environment (HasAuthCookieName (authCookieName))
import Handlers.Global (errorToast)
import Html.Login qualified as Login
import Lucid (renderText)
import Model (
  ExpirationTime (..),
  SessionId (unSessionId),
  User (..),
 )
import MyUUID qualified
import Password qualified
import Relude
import Web.Scotty.Cookie (
  SetCookie (
    setCookieHttpOnly,
    setCookieMaxAge,
    setCookieName,
    setCookieSecure,
    setCookieValue
  ),
  defaultSetCookie,
  setCookie,
 )
import Web.Scotty.Trans (ActionT, formParam, html, setHeader)

getLogin :: (MonadIO m) => ActionT m ()
getLogin = html $ renderText Login.fullPage

postLogin ::
  ( HasAuthCookieName env
  , MonadReader env m
  , MonadSession m
  , MonadIO m
  , MonadUser m
  , MonadTime m
  ) =>
  ActionT m ()
postLogin = do
  cookieName <- lift $ asks authCookieName
  email <- formParam "email"
  password <- formParam "password"
  mUser <- getUser email
  let errorResponse = errorToast "There was a problem logging in, please try again."
  case mUser of
    Just user -> do
      let isValid = Password.validatePassword user.passwordHash password
      if isValid
        then do
          expirationTime <- ExpirationTime . addUTCTime (secondsToNominalDiffTime (20 * 60)) <$> now
          sessionId <- newSession email expirationTime
          setCookie
            defaultSetCookie
              { setCookieName = encodeUtf8 cookieName
              , setCookieValue = encodeUtf8 $ MyUUID.toText $ unSessionId sessionId
              , setCookieMaxAge = Just 1200
              , setCookieHttpOnly = True
              , setCookieSecure = True
              }
          setHeader "HX-Redirect" "/"
        else errorResponse
    _ -> errorResponse