module Handlers.Login where

import Data.Time (
  addUTCTime,
  getCurrentTime,
  secondsToNominalDiffTime,
 )
import Db qualified
import Environment (HasAppEnvironment, HasAuthCookieName (authCookieName), HasDbPath)
import Handlers.Global (errorToast)
import Html.Login qualified as Login
import Lucid (renderText)
import Model (
  SessionId (unSessionId),
  User (passwordHash),
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

postLogin :: (HasAuthCookieName env, HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => ActionT m ()
postLogin = do
  cookieName <- lift $ asks authCookieName
  email <- formParam "email"
  password <- formParam "password"
  eUser <- lift $ Db.getUserByEmail email
  let errorResponse = errorToast "There was a problem logging in, please try again."
  case eUser of
    Right (Just user) -> do
      let isValid = Password.validatePassword user.passwordHash password
      if isValid
        then do
          expirationTime <- liftIO $ addUTCTime (secondsToNominalDiffTime (20 * 60)) <$> getCurrentTime
          eSessionId <- lift $ Db.createSession email expirationTime
          case eSessionId of
            Right sessionId -> do
              setCookie
                defaultSetCookie
                  { setCookieName = encodeUtf8 cookieName
                  , setCookieValue = encodeUtf8 $ MyUUID.toText $ unSessionId sessionId
                  , setCookieMaxAge = Just 1200
                  , setCookieHttpOnly = True
                  , setCookieSecure = True
                  }
              setHeader "HX-Redirect" "/"
            Left _ -> errorResponse
        else errorResponse
    _ -> errorResponse