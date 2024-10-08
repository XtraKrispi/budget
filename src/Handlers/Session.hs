module Handlers.Session where

import AppError
import Control.Monad.Error.Class (MonadError)
import Effects.Session (MonadSession (logout))
import Environment (HasAuthCookieName (authCookieName))
import Model (SessionId (..), User)
import MyUUID qualified
import Relude
import Web.Scotty.Cookie (SetCookie (..), defaultSetCookie, getCookie, setCookie)
import Web.Scotty.Trans (ActionT, setHeader)

deleteSession ::
  ( MonadError AppError m
  , MonadSession m
  , MonadIO m
  , MonadReader env m
  , HasAuthCookieName env
  ) =>
  User ->
  ActionT m ()
deleteSession _ = do
  cookieName <- lift $ asks authCookieName
  cookie <- getCookie cookieName
  case cookie of
    Just val -> do
      case fmap SessionId (MyUUID.fromText val) of
        Just sessionId -> do
          _ <- Effects.Session.logout sessionId
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
