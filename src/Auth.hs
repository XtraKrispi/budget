module Auth where

import Data.Time (
  addUTCTime,
  diffUTCTime,
  nominalDiffTimeToSeconds,
  secondsToNominalDiffTime,
 )
import Effects.Auth
import Effects.Session (MonadSession (getSessionUser, slideSession))
import Effects.Time (MonadTime (now))
import Effects.WebServer
import Htmx.Request qualified as Htmx
import Model (ExpirationTime (..), SessionId (..), User)
import Relude

validateCookie ::
  ( MonadTime m
  , MonadSession m
  ) =>
  SessionId ->
  m (Maybe User)
validateCookie sessionId = do
  mUser <- getSessionUser sessionId
  case mUser of
    Nothing -> pure Nothing
    Just (user, ExpirationTime expiration) -> do
      n <- now
      let difference = nominalDiffTimeToSeconds $ diffUTCTime expiration n
      if difference >= 0
        then do
          when ((difference * 60) <= 10) $
            slideSession sessionId (ExpirationTime (addUTCTime (secondsToNominalDiffTime (20 * 60)) n))
          pure $ Just user
        else do
          pure Nothing

requiresAuth ::
  ( MonadTime m
  , MonadSession m
  , MonadWebServer m
  , MonadAuth m
  ) =>
  (User -> m ()) ->
  m ()
requiresAuth route = do
  mSessionId <- getAuthCookie
  case mSessionId of
    Just sessionId -> do
      cookieResult <- validateCookie sessionId
      case cookieResult of
        Just user ->
          route user
        Nothing ->
          redirect "/login"
    Nothing ->
      -- Redirect to /login, but depends on how we got here (HTMX?)
      redirect "/login"

redirectTo :: (MonadWebServer m) => Text -> m ()
redirectTo url = do
  htmx <- Htmx.isHtmx
  if htmx
    then
      setResponseHeader "HX-Location" url
    else
      redirect url
