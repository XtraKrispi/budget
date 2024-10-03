module Handlers.ResetPassword where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Random (MonadRandom)
import Control.Monad.Reader (MonadReader, MonadTrans (..), asks)
import Data.Time (
  addUTCTime,
  getCurrentTime,
  secondsToNominalDiffTime,
 )
import Db qualified
import Environment (
  BaseUrl (BaseUrl),
  HasAppEnvironment,
  HasBaseUrl (..),
  HasDbPath,
  HasSmtp (..),
  Smtp (
    smtpFromEmail,
    smtpFromName,
    smtpHostname,
    smtpPassword,
    smtpUsername
  ),
 )
import Html.Common (addToast)
import Html.ResetPassword qualified as ResetPassword
import Lucid (button_, class_, disabled_, id_, renderText, span_, type_)
import Model (AlertType (..), Email (unEmail), Password, PlainText, Token (Token), User (..))
import Network.Mail.Mime (htmlPart)
import Network.Mail.SMTP (Address (..), sendMailWithLoginTLS, simpleMail)
import Network.URI.Encode (encodeText)
import Password qualified
import ResetPassword qualified
import Web.Scotty.Trans (ActionT, captureParam, formParam, html, setHeader)

getResetPassword :: (MonadIO m) => ActionT m ()
getResetPassword = html $ renderText ResetPassword.landingPage

postResetPassword ::
  ( HasDbPath env
  , HasAppEnvironment env
  , HasSmtp env
  , HasBaseUrl env
  , MonadIO m
  , MonadReader env m
  , MonadRandom m
  ) =>
  ActionT m ()
postResetPassword = do
  smtpConfig <- lift $ asks smtp
  email <- formParam "email"
  (Token clearTextToken, hashedToken) <- lift ResetPassword.generateToken
  BaseUrl base <- lift $ asks baseUrl
  let url = base <> "/reset-password/" <> encodeText clearTextToken
  expiry <- addUTCTime (secondsToNominalDiffTime (10 * 60)) <$> liftIO getCurrentTime
  _ <- lift $ Db.insertResetToken email hashedToken expiry
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

getResetPasswordToken :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => ActionT m ()
getResetPasswordToken = do
  token :: Token PlainText <- captureParam "token"
  now <- liftIO getCurrentTime
  mUser <- fmap (ResetPassword.getUser token now) <$> lift Db.getUsersForResetPassword
  case mUser of
    Right (Just _user) ->
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
  ( HasDbPath env
  , HasAppEnvironment env
  , MonadIO m
  , MonadReader env m
  ) =>
  ActionT m ()
postResetPasswordToken = do
  token :: Token PlainText <- captureParam "token"
  now <- liftIO getCurrentTime
  password <- formParam "password"
  passwordConfirmation <- formParam "password-confirmation"
  mUser <- fmap (ResetPassword.getUser token now) <$> lift Db.getUsersForResetPassword
  case mUser of
    Right (Just user) ->
      if password == passwordConfirmation
        then do
          hashed <- Password.hashPassword password
          results <- lift $ runExceptT $ do
            ExceptT $ Db.updateUserPassword user.email hashed
            ExceptT $ Db.removeAllUserTokens user.email
          case results of
            Right () -> do
              setHeader "HX-Redirect" "/login"
              html $ renderText $ addToast Success (span_ "You have successfully reset your password, you may now log in.")
            Left _ -> do
              setHeader "HX-Reswap" "none"
              html $ renderText $ addToast Error (span_ "Something went wrong, please try again.")
        else do
          setHeader "HX-Reswap" "none"
          html $ renderText $ addToast Error (span_ "Something went wrong, please try again.")
    _ -> do
      setHeader "HX-Reswap" "none"
      html $ renderText $ addToast Error (span_ "Something went wrong, please try again.")
