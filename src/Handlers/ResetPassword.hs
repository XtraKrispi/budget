module Handlers.ResetPassword where

import Data.Time (
  addUTCTime,
  secondsToNominalDiffTime,
 )
import Effects.Config
import Effects.Mail
import Effects.Password
import Effects.ResetPassword
import Effects.Time (MonadTime)
import Effects.Time qualified
import Effects.User
import Effects.WebServer
import Environment (
  BaseUrl (BaseUrl),
 )
import Handlers.Global (errorToast, unknownErrorToast)
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
import Relude
import ResetPassword qualified

getResetPassword :: (MonadWebServer m) => m ()
getResetPassword = serveHtml ResetPassword.landingPage

postResetPassword ::
  ( MonadWebServer m
  , MonadResetPassword m
  , MonadTime m
  , MonadMail m
  , MonadConfig m
  ) =>
  m ()
postResetPassword = do
  mEmail <- fromForm "email"
  case mEmail of
    Just email -> do
      (Token clearTextToken, hashedToken) <- Effects.ResetPassword.generateToken
      BaseUrl base <- getBaseUrl
      let url = base <> "/reset-password/" <> encodeText clearTextToken
      expiry <- ExpirationTime . addUTCTime (secondsToNominalDiffTime (10 * 60)) <$> Effects.Time.now
      Effects.ResetPassword.insertToken email expiry hashedToken
      sendMail email (Subject "Budget - Reset Password") (ResetPassword.resetPasswordEmail url)
      setResponseHeader "HX-Reswap" "none"
      serveHtml $ addToast Success (span_ "If you have an account with Budget, please check your email for a password reset link.")
    Nothing -> errorToast "An unknown error has occurred. Please try again."

getResetPasswordToken ::
  ( MonadUser m
  , MonadWebServer m
  , MonadTime m
  , MonadResetPassword m
  ) =>
  m ()
getResetPasswordToken = do
  mToken <- fromUrl "token"
  case mToken of
    Just token -> do
      now <- Effects.Time.now
      mUser <- ResetPassword.getUser token now <$> Effects.ResetPassword.getUsers
      case mUser of
        Just _user ->
          serveHtml $ ResetPassword.tokenPage token
        _ -> serveHtml ResetPassword.errorPage
    Nothing -> unknownErrorToast

postResetPasswordValidate :: (MonadWebServer m) => m ()
postResetPasswordValidate = do
  formData <- runMaybeT $ do
    password :: Password PlainText <- MaybeT $ fromForm "password"
    passwordConfirmation <- MaybeT $ fromForm "password-confirmation"
    pure (password, passwordConfirmation)
  case formData of
    Nothing -> unknownErrorToast
    Just (password, passwordConfirmation) ->
      if password /= passwordConfirmation
        then serveHtml $ do
          ResetPassword.passwordConfirmationError True True
          button_
            [ class_ "btn btn-primary"
            , type_ "submit"
            , id_ "change-password"
            , disabled_ "disabled"
            ]
            "Reset Password"
        else do
          serveHtml $ do
            ResetPassword.passwordConfirmationError True False
            button_
              [ class_ "btn btn-primary"
              , type_ "submit"
              , id_ "change-password"
              ]
              "Reset Password"

postResetPasswordToken ::
  ( MonadWebServer m
  , MonadResetPassword m
  , MonadUser m
  , MonadTime m
  , MonadPassword m
  ) =>
  m ()
postResetPasswordToken = do
  formData <- runMaybeT $ do
    token <- MaybeT $ fromUrl "token"
    password :: Password PlainText <- MaybeT $ fromForm "password"
    passwordConfirmation <- MaybeT $ fromForm "password-confirmation"
    pure (token, password, passwordConfirmation)
  case formData of
    Just (token, password, passwordConfirmation) -> do
      now <- Effects.Time.now
      mUser <- ResetPassword.getUser token now <$> Effects.ResetPassword.getUsers
      let errorResponse = errorToast "Something went wrong, please try again."
      case mUser of
        (Just user) ->
          if password == passwordConfirmation
            then do
              hashed <- Effects.Password.hashPassword password
              Effects.User.updatePassword user.email hashed
              Effects.ResetPassword.removeUserTokens user.email
              setResponseHeader "HX-Redirect" "/login"
              serveHtml $ addToast Success (span_ "You have successfully reset your password, you may now log in.")
            else errorResponse
        _ -> errorResponse
    Nothing -> unknownErrorToast
