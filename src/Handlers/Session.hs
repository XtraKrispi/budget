module Handlers.Session where

import Db qualified
import Environment (HasAppEnvironment, HasAuthCookieName (authCookieName), HasDbPath)
import Model (SessionId (..))
import MyUUID qualified
import Relude
import Web.Scotty.Auth (requiresAuth)
import Web.Scotty.Cookie (SetCookie (..), defaultSetCookie, getCookie, setCookie)
import Web.Scotty.Trans (ActionT, setHeader)

deleteSession :: (HasAuthCookieName env, HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => ActionT m ()
deleteSession = requiresAuth \_user -> do
  cookieName <- lift $ asks authCookieName
  cookie <- getCookie cookieName
  case cookie of
    Just val -> do
      case MyUUID.fromText val of
        Just uuid -> do
          let sessionId = SessionId uuid
          _ <- lift $ Db.deleteSession sessionId
          setCookie
            defaultSetCookie
              { setCookieName = encodeUtf8 cookieName
              , setCookieValue = encodeUtf8 val
              , setCookieMaxAge = Just (-100)
              , setCookieHttpOnly = True
              , setCookieSecure = True
              }
        Nothing -> pure ()
    Nothing -> pure ()
  setHeader "HX-Redirect" "/"
