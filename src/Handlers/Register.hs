module Handlers.Register where

import Effects.Password
import Effects.User
import Effects.WebServer
import Handlers.Global (errorToast)
import Html.Common (addToast)
import Html.Login qualified as Login
import Htmx.Attributes
import Lucid
import Model
import Relude

postRegister ::
  ( MonadWebServer m
  , MonadUser m
  , MonadPassword m
  ) =>
  m ()
postRegister = do
  formData <- runMaybeT $ do
    emailAddress <- MaybeT $ fromForm "register-email"
    password :: Password PlainText <- MaybeT $ fromForm "register-password"
    passwordConfirmation <- MaybeT $ fromForm "register-password-confirm"
    name <- MaybeT $ fromForm "register-name"
    pure (emailAddress, name, password, passwordConfirmation)
  case formData of
    Just (emailAddress, name, password, passwordConfirmation) -> do
      mUser <- Effects.User.getUser emailAddress
      if isNothing mUser && password == passwordConfirmation
        then do
          hashed <- hashPassword password
          Effects.User.insert (User emailAddress name hashed)
          setResponseHeader "HX-Trigger" "hideRegistrationModal"
          serveHtml $ addToast Success (span_ "You've been signed up! Please log in below")
        else serveHtml do
          -- Validation problems
          when (isJust mUser) do
            Login.emailTakenError True True
          when (password /= passwordConfirmation) do
            Login.passwordConfirmationError True True
          button_
            [ class_ "btn btn-primary"
            , type_ "submit"
            , id_ "sign-up"
            , hxSwapOob "outerHTML"
            , disabled_ "disabled"
            ]
            "Sign Up"
    Nothing -> errorToast "An unknown error occurred, please try again."

postRegisterValidate ::
  ( MonadWebServer m
  , MonadUser m
  ) =>
  m ()
postRegisterValidate = do
  isEmail <- (== Just "register-email") <$> getRequestHeader "HX-Trigger-Name"
  formData <- runMaybeT $ do
    emailAddress <- MaybeT $ fromForm "register-email"
    password :: Password PlainText <- MaybeT $ fromForm "register-password"
    passwordConfirmation <- MaybeT $ fromForm "register-password-confirm"
    pure (emailAddress, password, passwordConfirmation)
  case formData of
    Just (emailAddress, password, passwordConfirmation) -> do
      mUser <- Effects.User.getUser emailAddress
      let isValid = isNothing mUser && password == passwordConfirmation
      if isValid
        then do
          setResponseHeader "HX-Reswap" "none"
          serveHtml $ do
            Login.emailTakenError True False
            Login.passwordConfirmationError True False
            button_
              [ class_ "btn btn-primary"
              , type_ "submit"
              , id_ "sign-up"
              , hxSwapOob "outerHTML"
              ]
              "Sign Up"
        else serveHtml do
          Login.emailTakenError (not isEmail) (isJust mUser)
          Login.passwordConfirmationError isEmail (password /= passwordConfirmation)
          button_
            [ class_ "btn btn-primary"
            , type_ "submit"
            , id_ "sign-up"
            , hxSwapOob "outerHTML"
            , disabled_ "disabled"
            ]
            "Sign Up"
    Nothing -> errorToast "An unknown error occurred, please try again."
