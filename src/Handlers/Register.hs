module Handlers.Register where

import Effects.User
import Html.Common (addToast)
import Html.Login qualified as Login
import Htmx.Attributes
import Lucid
import Model
import Password qualified
import Relude
import Web.Scotty.Trans

postRegister ::
  ( MonadIO m
  , MonadUser m
  ) =>
  ActionT m ()
postRegister = do
  emailAddress <- formParam "register-email"
  password <- formParam "register-password"
  passwordConfirmation <- formParam "register-password-confirm"
  name <- formParam "register-name"
  mUser <- Effects.User.getUser emailAddress
  if isNothing mUser && password == passwordConfirmation
    then do
      hashed <- Password.hashPassword password
      Effects.User.insert (User emailAddress name hashed)
      setHeader "HX-Trigger" "hideRegistrationModal"
      html $ renderText $ addToast Success (span_ "You've been signed up! Please log in below")
    else html $ renderText do
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

postRegisterValidate ::
  ( MonadIO m
  , MonadUser m
  ) =>
  ActionT m ()
postRegisterValidate = do
  isEmail <- (== Just "register-email") <$> header "HX-Trigger-Name"
  emailAddress <- formParam "register-email"
  password :: Password PlainText <- formParam "register-password"
  passwordConfirmation :: Password PlainText <- formParam "register-password-confirm"
  mUser <- Effects.User.getUser emailAddress
  let isValid = isNothing mUser && password == passwordConfirmation
  if isValid
    then do
      setHeader "HX-Reswap" "none"
      html $
        renderText $ do
          Login.emailTakenError True False
          Login.passwordConfirmationError True False
          button_
            [ class_ "btn btn-primary"
            , type_ "submit"
            , id_ "sign-up"
            , hxSwapOob "outerHTML"
            ]
            "Sign Up"
    else html $ renderText do
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
