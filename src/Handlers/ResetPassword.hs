module Handlers.ResetPassword where

import Data.Time (
  addUTCTime,
  getCurrentTime,
  secondsToNominalDiffTime,
 )
import Effects.ResetPassword
import Effects.Time (MonadTime)
import Effects.Time qualified
import Effects.User
import Environment (
  BaseUrl (BaseUrl),
  HasBaseUrl (..),
  HasSmtp (..),
  Smtp (
    smtpFromEmail,
    smtpFromName,
    smtpHostname,
    smtpPassword,
    smtpUsername
  ),
 )
import Handlers.Global (errorToast)
import Html.Common (addToast)
import Html.ResetPassword qualified as ResetPassword
import Lucid (button_, class_, disabled_, id_, renderText, span_, type_)
import Model (AlertType (..), Email (unEmail), ExpirationTime (..), Password, PlainText, Token (Token), User (..))
import Network.Mail.Mime (htmlPart)
import Network.Mail.SMTP (Address (..), sendMailWithLoginTLS, simpleMail)
import Network.URI.Encode (encodeText)
import Password qualified
import Relude
import ResetPassword qualified
import Web.Scotty.Trans (ActionT, captureParam, formParam, html, setHeader)

getResetPassword :: (MonadIO m) => ActionT m ()
getResetPassword = html $ renderText ResetPassword.landingPage

postResetPassword ::
  ( HasSmtp env
  , HasBaseUrl env
  , MonadIO m
  , MonadReader env m
  , MonadResetPassword m
  , MonadTime m
  ) =>
  ActionT m ()
postResetPassword = do
  smtpConfig <- lift $ asks smtp
  email <- formParam "email"
  (Token clearTextToken, hashedToken) <- Effects.ResetPassword.generateToken
  BaseUrl base <- lift $ asks baseUrl
  let url = base <> "/reset-password/" <> encodeText clearTextToken
  expiry <- ExpirationTime . addUTCTime (secondsToNominalDiffTime (10 * 60)) <$> Effects.Time.now
  Effects.ResetPassword.insertToken email expiry hashedToken
  let mail =
        simpleMail
          ( Address
              (Just smtpConfig.smtpFromName)
              smtpConfig.smtpFromEmail
          )
          [Address Nothing (unEmail email)]
          []
          []
          "Budget - Reset Password"
          [htmlPart (renderText (ResetPassword.resetPasswordEmail url))]
  liftIO $ sendMailWithLoginTLS smtpConfig.smtpHostname smtpConfig.smtpUsername smtpConfig.smtpPassword mail
  setHeader "HX-Reswap" "none"
  html $ renderText $ addToast Success (span_ "If you have an account with Budget, please check your email for a password reset link.")

getResetPasswordToken ::
  ( MonadUser m
  , MonadIO m
  , MonadTime m
  , MonadResetPassword m
  ) =>
  ActionT m ()
getResetPasswordToken = do
  token :: Token PlainText <- captureParam "token"
  now <- Effects.Time.now
  mUser <- ResetPassword.getUser token now <$> Effects.ResetPassword.getUsers
  case mUser of
    Just _user ->
      html $ renderText $ ResetPassword.tokenPage token
    _ -> html $ renderText ResetPassword.errorPage

postResetPasswordValidate :: (MonadIO m) => ActionT m ()
postResetPasswordValidate = do
  password :: Password PlainText <- formParam "password"
  passwordConfirmation :: Password PlainText <- formParam "password-confirmation"
  if password /= passwordConfirmation
    then html $
      renderText $ do
        ResetPassword.passwordConfirmationError True True
        button_
          [ class_ "btn btn-primary"
          , type_ "submit"
          , id_ "change-password"
          , disabled_ "disabled"
          ]
          "Reset Password"
    else do
      html $
        renderText $ do
          ResetPassword.passwordConfirmationError True False
          button_
            [ class_ "btn btn-primary"
            , type_ "submit"
            , id_ "change-password"
            ]
            "Reset Password"

postResetPasswordToken ::
  ( MonadIO m
  , MonadResetPassword m
  , MonadUser m
  ) =>
  ActionT m ()
postResetPasswordToken = do
  token :: Token PlainText <- captureParam "token"
  now <- liftIO getCurrentTime
  password <- formParam "password"
  passwordConfirmation <- formParam "password-confirmation"
  mUser <- ResetPassword.getUser token now <$> Effects.ResetPassword.getUsers
  let errorResponse = errorToast "Something went wrong, please try again."
  case mUser of
    (Just user) ->
      if password == passwordConfirmation
        then do
          hashed <- Password.hashPassword password
          Effects.User.updatePassword user.email hashed
          Effects.ResetPassword.removeUserTokens user.email
          setHeader "HX-Redirect" "/login"
          html $ renderText $ addToast Success (span_ "You have successfully reset your password, you may now log in.")
        else errorResponse
    _ -> errorResponse
