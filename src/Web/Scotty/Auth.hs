module Web.Scotty.Auth where

import Data.Time (
  addUTCTime,
  diffUTCTime,
  nominalDiffTimeToSeconds,
  secondsToNominalDiffTime,
 )
import Effects.Session (MonadSession (getSessionUser, slideSession))
import Effects.Time (MonadTime (now))
import Environment (HasAuthCookieName (authCookieName))
import Htmx.Request qualified as Htmx
import Model (ExpirationTime (..), SessionId (..), User)
import MyUUID qualified
import Relude
import Web.Scotty.Cookie (getCookie)
import Web.Scotty.Trans (ActionT, redirect, setHeader)

validateCookie ::
  ( MonadTime m
  , MonadSession m
  ) =>
  Text ->
  m (Maybe User)
validateCookie cookieVal =
  case fmap SessionId (MyUUID.fromText cookieVal) of
    Just sessionId -> do
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
    Nothing -> do
      pure Nothing

requiresAuth ::
  ( MonadReader env m
  , MonadIO m
  , HasAuthCookieName env
  , MonadTime m
  , MonadSession m
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
        Just user ->
          route user
        Nothing ->
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
