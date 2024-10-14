module Handlers.Auth where

import Control.Monad (when)
import Data.Foldable (find)
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
import Handlers.Model
import Handlers.Utils (redirectTo)
import Model.Common
import Model.MyUUID qualified as MyUUID
import Model.User

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

getAuthCookie :: (Reader Environment :> es) => Request -> Eff es (Maybe SessionId)
getAuthCookie request = do
  name <- asks envAuthCookieName
  let mVal = snd <$> find (\(n, _) -> n == name) request.requestCookies
  case mVal >>= MyUUID.fromText of
    Just i -> pure $ Just (SessionId i)
    Nothing -> pure Nothing

invalidateAuthCookie :: (Reader Environment :> es) => Eff es Cookie
invalidateAuthCookie = do
  cookieName <- asks envAuthCookieName
  pure $
    Cookie
      { cookieName = cookieName
      , cookieValue = ""
      , cookieMaxAge = Just (-100)
      , cookieHttpOnly = True
      , cookieSecure = True
      }

newAuthCookie :: (Reader Environment :> es) => SessionId -> Eff es Cookie
newAuthCookie sessionId = do
  cookieName <- asks envAuthCookieName
  pure $
    Cookie
      { cookieName = cookieName
      , cookieValue = MyUUID.toText $ unSessionId sessionId
      , cookieMaxAge = Just 1200
      , cookieHttpOnly = True
      , cookieSecure = True
      }

requiresAuth ::
  ( Time :> es
  , SessionStore :> es
  , Reader Environment :> es
  ) =>
  (Request -> User -> Eff es Response) ->
  Request ->
  Eff es Response
requiresAuth route request = do
  mSessionId <- getAuthCookie request
  case mSessionId of
    Just sessionId -> do
      cookieResult <- validateCookie sessionId
      case cookieResult of
        Just user ->
          route request user
        Nothing ->
          pure $ redirectTo request "/login"
    Nothing ->
      pure $ redirectTo request "/login"
