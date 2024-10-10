module Auth where

import Control.Monad (when)
import Control.Monad.Trans (lift)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (
  addUTCTime,
  diffUTCTime,
  nominalDiffTimeToSeconds,
  secondsToNominalDiffTime,
 )
import Effectful
import Effectful.Reader.Static (Reader, asks)
import Effects.SessionStore
import Effects.Time
import Environment (Environment (envAuthCookieName))
import Model (ExpirationTime (..), SessionId (..), User)
import MyUUID
import Web.Scotty.ActionT (redirectTo)
import Web.Scotty.Cookie (SetCookie (..), defaultSetCookie, getCookie, setCookie)
import Web.Scotty.Trans (ActionT)

validateCookie ::
  ( Time :> es
  , SessionStore :> es
  ) =>
  SessionId ->
  Eff es (Maybe User)
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

-- getAuthCookie ::
--   ( Reader env :> es
--   , HasAuthCookieName env
--   ) =>
getAuthCookie :: (Reader Environment :> es) => ActionT (Eff es) (Maybe SessionId)
getAuthCookie = do
  cookieName <- lift $ asks envAuthCookieName
  val <- getCookie cookieName
  case val >>= MyUUID.fromText of
    Just i -> pure $ Just (SessionId i)
    Nothing -> pure Nothing

invalidateAuthCookie :: (IOE :> es, Reader Environment :> es) => ActionT (Eff es) ()
invalidateAuthCookie = do
  cookieName <- lift $ asks envAuthCookieName
  setCookie
    defaultSetCookie
      { setCookieName = encodeUtf8 cookieName
      , setCookieValue = ""
      , setCookieMaxAge = Just (-100)
      , setCookieHttpOnly = True
      , setCookieSecure = True
      }

setAuthCookie :: (IOE :> es, Reader Environment :> es) => SessionId -> ActionT (Eff es) ()
setAuthCookie sessionId = do
  cookieName <- lift $ asks envAuthCookieName
  setCookie
    defaultSetCookie
      { setCookieName = encodeUtf8 cookieName
      , setCookieValue = encodeUtf8 $ MyUUID.toText $ unSessionId sessionId
      , setCookieMaxAge = Just 1200
      , setCookieHttpOnly = True
      , setCookieSecure = True
      }

requiresAuth ::
  ( Time :> es
  , SessionStore :> es
  , Reader Environment :> es
  , IOE :> es
  ) =>
  (User -> ActionT (Eff es) ()) ->
  ActionT (Eff es) ()
requiresAuth route = do
  mSessionId <- getAuthCookie
  case mSessionId of
    Just sessionId -> do
      cookieResult <- lift $ validateCookie sessionId
      case cookieResult of
        Just user ->
          route user
        Nothing ->
          redirectTo "/login"
    Nothing ->
      redirectTo "/login"
