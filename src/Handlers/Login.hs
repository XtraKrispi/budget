module Handlers.Login where

import Data.Time (
  addUTCTime,
  secondsToNominalDiffTime,
 )
import Effects.Auth (MonadAuth (setAuthCookie))
import Effects.Session
import Effects.Time
import Effects.User (MonadUser (..))
import Effects.WebServer
import Handlers.Global (errorToast)
import Html.Login qualified as Login
import Model (
  ExpirationTime (..),
  User (..),
 )
import Password qualified
import Relude

getLogin :: (MonadWebServer m) => m ()
getLogin = serveHtml Login.fullPage

postLogin ::
  ( MonadSession m
  , MonadWebServer m
  , MonadUser m
  , MonadTime m
  , MonadAuth m
  ) =>
  m ()
postLogin = do
  mEmail <- fromForm "email"
  mPassword <- fromForm "password"

  case (mEmail, mPassword) of
    (Just email, Just password) -> do
      mUser <- getUser email

      case mUser of
        Just user -> do
          let isValid = Password.validatePassword user.passwordHash password
          if isValid
            then do
              expirationTime <- ExpirationTime . addUTCTime (secondsToNominalDiffTime (20 * 60)) <$> now
              sessionId <- newSession email expirationTime
              setAuthCookie sessionId
              setResponseHeader "HX-Redirect" "/"
            else errorToast "There was a problem logging in, please try again."
        _ -> errorToast "There was a problem logging in, please try again."
    _ -> errorToast "There was a problem logging in, please try again."