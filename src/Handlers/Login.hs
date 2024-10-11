module Handlers.Login where

import AppError
import Data.Time (
  addUTCTime,
  secondsToNominalDiffTime,
 )
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader, asks)
import Effects.SessionStore
import Effects.Time
import Effects.UserStore
import Environment (Environment (envAuthCookieName))
import Handlers.Model
import Handlers.Utils (errorResponse, getParam, htmlResponse, makeResponse)
import Html.Login qualified as Login
import Model (
  ExpirationTime (..),
  SessionId (unSessionId),
  User (..),
 )
import MyUUID qualified
import Password qualified

getLogin :: Eff es Response
getLogin = pure $ htmlResponse Login.fullPage

postLogin ::
  ( Reader Environment :> es
  , Time :> es
  , SessionStore :> es
  , UserStore :> es
  , Error AppError :> es
  ) =>
  Request ->
  Eff es Response
postLogin req = do
  cookieName <- asks envAuthCookieName
  email <- getParam req "email"
  password <- getParam req "password"
  mUser <- Effects.UserStore.get email
  case mUser of
    Just user -> do
      let isValid = Password.validatePassword user.passwordHash password
      if isValid
        then do
          expirationTime <- ExpirationTime . addUTCTime (secondsToNominalDiffTime (20 * 60)) <$> now
          sessionId <- newSession email expirationTime
          pure $
            makeResponse
              [("HX-Redirect", "/")]
              [ Cookie
                  { cookieName = cookieName
                  , cookieValue = MyUUID.toText $ unSessionId sessionId
                  , cookieMaxAge = Just 1200
                  , cookieHttpOnly = True
                  , cookieSecure = True
                  }
              ]
              mempty
        else
          pure $ errorResponse "There was a problem logging in, please try again."
    _ ->
      pure $ errorResponse "There was a problem logging in, please try again."
