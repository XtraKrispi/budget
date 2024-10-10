module Handlers.ResetPassword where

import Control.Monad.Trans (lift)
import Data.Time (
  addUTCTime,
  secondsToNominalDiffTime,
 )
import Effectful
import Effectful.Reader.Static (Reader, asks)
import Effects.HashPassword
import Effects.Mail
import Effects.ResetPasswordStore
import Effects.Time (Time)
import Effects.Time qualified
import Effects.UserStore
import Environment (
  BaseUrl (BaseUrl),
  Environment (envBaseUrl),
 )
import Handlers.Global (errorToast)
import Html.Common (addToast)
import Html.ResetPassword qualified as ResetPassword
import Lucid (button_, class_, disabled_, id_, span_, type_)
import Model (
  AlertType (..),
  ExpirationTime (..),
  Password,
  PlainText,
  Token (Token),
  User (..),
 )
import Network.URI.Encode (encodeText)
import ResetPassword qualified
import Web.Scotty.ActionT (renderHtml)
import Web.Scotty.Trans (ActionT, captureParam, formParam, setHeader)

getResetPassword :: (IOE :> es) => ActionT (Eff es) ()
getResetPassword = renderHtml ResetPassword.landingPage

postResetPassword ::
  ( ResetPasswordStore :> es
  , Time :> es
  , Mail :> es
  , Reader Environment :> es
  , IOE :> es
  ) =>
  ActionT (Eff es) ()
postResetPassword = do
  email <- formParam "email"
  (Token clearTextToken, hashedToken) <- lift Effects.ResetPasswordStore.generateToken
  BaseUrl base <- lift $ asks envBaseUrl
  let url = base <> "/reset-password/" <> encodeText clearTextToken
  expiry <- lift $ ExpirationTime . addUTCTime (secondsToNominalDiffTime (10 * 60)) <$> Effects.Time.now
  lift $ Effects.ResetPasswordStore.insertToken email expiry hashedToken
  lift $ sendMail email (Subject "Budget - Reset Password") (ResetPassword.resetPasswordEmail url)
  setHeader "HX-Reswap" "none"
  renderHtml $ addToast Success (span_ "If you have an account with Budget, please check your email for a password reset link.")

getResetPasswordToken ::
  ( Time :> es
  , ResetPasswordStore :> es
  , IOE :> es
  ) =>
  ActionT (Eff es) ()
getResetPasswordToken = do
  token <- captureParam "token"
  now <- lift Effects.Time.now
  mUser <- lift $ ResetPassword.getUser token now <$> Effects.ResetPasswordStore.getUsers
  case mUser of
    Just _user ->
      renderHtml $ ResetPassword.tokenPage token
    _ -> renderHtml ResetPassword.errorPage

postResetPasswordValidate :: (IOE :> es) => ActionT (Eff es) ()
postResetPasswordValidate = do
  password :: Password PlainText <- formParam "password"
  passwordConfirmation <- formParam "password-confirmation"
  if password /= passwordConfirmation
    then renderHtml $ do
      ResetPassword.passwordConfirmationError True True
      button_
        [ class_ "btn btn-primary"
        , type_ "submit"
        , id_ "change-password"
        , disabled_ "disabled"
        ]
        "Reset Password"
    else do
      renderHtml $ do
        ResetPassword.passwordConfirmationError True False
        button_
          [ class_ "btn btn-primary"
          , type_ "submit"
          , id_ "change-password"
          ]
          "Reset Password"

postResetPasswordToken ::
  ( ResetPasswordStore :> es
  , UserStore :> es
  , Time :> es
  , HashPassword :> es
  , IOE :> es
  ) =>
  ActionT (Eff es) ()
postResetPasswordToken = do
  token <- formParam "token"
  password :: Password PlainText <- formParam "password"
  passwordConfirmation <- formParam "password-confirmation"
  now <- lift Effects.Time.now
  mUser <- lift $ ResetPassword.getUser token now <$> Effects.ResetPasswordStore.getUsers
  let errorResponse = errorToast "Something went wrong, please try again."
  case mUser of
    (Just user) ->
      if password == passwordConfirmation
        then do
          hashed <- lift $ Effects.HashPassword.hashPassword password
          lift $ Effects.UserStore.updatePassword user.email hashed
          lift $ Effects.ResetPasswordStore.removeUserTokens user.email
          setHeader "HX-Redirect" "/login"
          renderHtml $ addToast Success (span_ "You have successfully reset your password, you may now log in.")
        else errorResponse
    _ -> errorResponse
