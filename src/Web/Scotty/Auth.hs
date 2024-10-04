module Web.Scotty.Auth where

import Data.Time (addUTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Db qualified
import Environment (HasAppEnvironment, HasAuthCookieName (authCookieName), HasDbPath)
import Htmx.Request qualified as Htmx
import Model (SessionId (..), User)
import MyUUID qualified
import Relude
import Web.Scotty.Cookie (getCookie)
import Web.Scotty.Trans (ActionT, redirect, setHeader)

validateCookie ::
  ( HasDbPath env
  , HasAppEnvironment env
  , MonadIO m
  , MonadReader env m
  ) =>
  Text ->
  m (Either Text User)
validateCookie cookieVal =
  case MyUUID.fromText cookieVal of
    Just uuid -> do
      let sessionId = SessionId uuid
      eUser <- Db.getUserForSession sessionId
      case eUser of
        Right (user, expiration) -> do
          now <- liftIO getCurrentTime
          let difference = nominalDiffTimeToSeconds $ diffUTCTime expiration now
          if difference >= 0
            then do
              when ((difference * 60) <= 10) do
                _ <- Db.updateSession sessionId (addUTCTime (secondsToNominalDiffTime (20 * 60)) now)
                pure ()
              pure $ Right user
            else do
              pure (Left "Session expired")
        _ -> pure (Left "Invalid session id")
    Nothing -> do
      pure (Left "Invalid session id")

requiresAuth ::
  ( HasDbPath env
  , HasAppEnvironment env
  , HasAuthCookieName env
  , MonadIO m
  , MonadReader env m
  ) =>
  (User -> ActionT m ()) ->
  ActionT m ()
requiresAuth route = do
  cookieName <- lift $ asks authCookieName
  mCookie <- getCookie cookieName
  case mCookie of
    Just cookie -> do
      cookieResult <- lift $ validateCookie cookie
      case cookieResult of
        Right user ->
          route user
        Left _ ->
          redirectTo "/login"
    Nothing ->
      -- Redirect to /login, but depends on how we got here (HTMX?)
      redirectTo "/login"

redirectTo :: (MonadIO m) => Text -> ActionT m ()
redirectTo url = do
  htmx <- Htmx.isHtmx
  if htmx
    then
      setHeader "HX-Location" (fromStrict url)
    else
      redirect (fromStrict url)
