module Handlers.ResetPassword where

import AppError (AppError)
import Data.Time (
  addUTCTime,
  secondsToNominalDiffTime,
 )
import Effectful
import Effectful.Error.Static (Error)
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
import Handlers.Model
import Handlers.Utils
import Html.Common (AlertType (..), addToast)
import Html.ResetPassword qualified as ResetPassword
import Lucid (button_, class_, disabled_, id_, span_, type_)
import Model.Common
import Model.Password
import Model.Token
import Model.User (User (..))
import Network.URI.Encode (encodeText)
import ResetPassword qualified

getResetPassword :: Eff es Response
getResetPassword = pure $ htmlResponse ResetPassword.landingPage

postResetPassword ::
  ( ResetPasswordStore :> es
  , Time :> es
  , Mail :> es
  , Reader Environment :> es
  , Error AppError :> es
  ) =>
  Request ->
  Eff es Response
postResetPassword request = do
  email <- getParam request "email"
  (Token clearTextToken, hashedToken) <- Effects.ResetPasswordStore.generateToken
  BaseUrl base <- asks envBaseUrl
  let url = base <> "/reset-password/" <> encodeText clearTextToken
  expiry <- ExpirationTime . addUTCTime (secondsToNominalDiffTime (10 * 60)) <$> Effects.Time.now
  Effects.ResetPasswordStore.insertToken email expiry hashedToken
  sendMail email (Subject "Budget - Reset Password") (ResetPassword.resetPasswordEmail url)
  pure $
    makeResponse [("HX-Reswap", "none")] [] $
      addToast
        Success
        (span_ "If you have an account with Budget, please check your email for a password reset link.")

getResetPasswordToken ::
  ( Time :> es
  , ResetPasswordStore :> es
  , Error AppError :> es
  ) =>
  Request ->
  Eff es Response
getResetPasswordToken request = do
  token <- getParam request "token"
  now <- Effects.Time.now
  mUser <- ResetPassword.getUser token now <$> Effects.ResetPasswordStore.getUsers
  case mUser of
    Just _user ->
      pure $ htmlResponse $ ResetPassword.tokenPage token
    _ -> pure $ htmlResponse ResetPassword.errorPage

postResetPasswordValidate :: (Error AppError :> es) => Request -> Eff es Response
postResetPasswordValidate request = do
  password :: Password PlainText <- getParam request "password"
  passwordConfirmation <- getParam request "password-confirmation"
  if password /= passwordConfirmation
    then pure $ htmlResponse do
      ResetPassword.passwordConfirmationError True True
      button_
        [ class_ "btn btn-primary"
        , type_ "submit"
        , id_ "change-password"
        , disabled_ "disabled"
        ]
        "Reset Password"
    else do
      pure $ htmlResponse do
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
  , Error AppError :> es
  ) =>
  Request ->
  Eff es Response
postResetPasswordToken request = do
  token <- getParam request "token"
  password :: Password PlainText <- getParam request "password"
  passwordConfirmation <- getParam request "password-confirmation"
  now <- Effects.Time.now
  mUser <- ResetPassword.getUser token now <$> Effects.ResetPasswordStore.getUsers
  let err = errorResponse "Something went wrong, please try again."
  case mUser of
    (Just user) ->
      if password == passwordConfirmation
        then do
          hashed <- Effects.HashPassword.hashPassword password
          Effects.UserStore.updatePassword user.email hashed
          Effects.ResetPasswordStore.removeUserTokens user.email
          pure $
            makeResponse [("HX-Redirect", "/login")] [] $
              addToast
                Success
                (span_ "You have successfully reset your password, you may now log in.")
        else pure err
    _ -> pure err
