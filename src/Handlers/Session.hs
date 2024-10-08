module Handlers.Session where

import Effects.Auth
import Effects.Session (MonadSession (logout))
import Effects.WebServer
import Model (User)
import Relude

deleteSession ::
  ( MonadSession m
  , MonadWebServer m
  , MonadAuth m
  ) =>
  User ->
  m ()
deleteSession _ = do
  mSessionId <- getAuthCookie
  case mSessionId of
    Just sessionId -> do
      Effects.Session.logout sessionId
      invalidateAuthCookie
    Nothing -> pure ()

  setResponseHeader "HX-Redirect" "/"
