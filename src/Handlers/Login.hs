module Handlers.Login where

import Auth qualified
import Control.Monad.Trans (lift)
import Data.Time (
  addUTCTime,
  secondsToNominalDiffTime,
 )
import Effectful
import Effectful.Reader.Static (Reader)
import Effects.SessionStore
import Effects.Time
import Effects.UserStore
import Environment (Environment)
import Handlers.Global (errorToast)
import Html.Login qualified as Login
import Model (
  ExpirationTime (..),
  User (..),
 )
import Password qualified
import Web.Scotty.ActionT (renderHtml)
import Web.Scotty.Trans (ActionT, formParam, setHeader)

getLogin :: (IOE :> es) => ActionT (Eff es) ()
getLogin = renderHtml Login.fullPage

postLogin ::
  ( SessionStore :> es
  , UserStore :> es
  , Time :> es
  , Reader Environment :> es
  , IOE :> es
  ) =>
  ActionT (Eff es) ()
postLogin = do
  email <- formParam "email"
  password <- formParam "password"
  mUser <- lift $ Effects.UserStore.get email

  case mUser of
    Just user -> do
      let isValid = Password.validatePassword user.passwordHash password
      if isValid
        then do
          expirationTime <- lift $ ExpirationTime . addUTCTime (secondsToNominalDiffTime (20 * 60)) <$> now
          sessionId <- lift $ newSession email expirationTime
          Auth.setAuthCookie sessionId
          setHeader "HX-Redirect" "/"
        else errorToast "There was a problem logging in, please try again."
    _ -> errorToast "There was a problem logging in, please try again."
