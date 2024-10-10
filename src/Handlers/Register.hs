module Handlers.Register where

import Control.Monad (when)
import Control.Monad.Trans (lift)
import Data.Maybe (isJust, isNothing)
import Effectful
import Effects.HashPassword
import Effects.UserStore
import Html.Common (addToast)
import Html.Login qualified as Login
import Htmx.Attributes
import Lucid
import Model
import Web.Scotty.ActionT (renderHtml)
import Web.Scotty.Trans

postRegister ::
  ( UserStore :> es
  , HashPassword :> es
  , IOE :> es
  ) =>
  ActionT (Eff es) ()
postRegister = do
  emailAddress <- formParam "register-email"
  password :: Password PlainText <- formParam "register-password"
  passwordConfirmation <- formParam "register-password-confirm"
  name <- formParam "register-name"
  mUser <- lift $ Effects.UserStore.get emailAddress
  if isNothing mUser && password == passwordConfirmation
    then do
      hashed <- lift $ hashPassword password
      lift $ Effects.UserStore.insert (User emailAddress name hashed)
      setHeader "HX-Trigger" "hideRegistrationModal"
      renderHtml $ addToast Success (span_ "You've been signed up! Please log in below")
    else renderHtml do
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
  ( UserStore :> es
  , IOE :> es
  ) =>
  ActionT (Eff es) ()
postRegisterValidate = do
  isEmail <- (== Just "register-email") <$> header "HX-Trigger-Name"
  emailAddress <- formParam "register-email"
  password :: Password PlainText <- formParam "register-password"
  passwordConfirmation <- formParam "register-password-confirm"
  mUser <- lift $ Effects.UserStore.get emailAddress
  let isValid = isNothing mUser && password == passwordConfirmation
  if isValid
    then do
      setHeader "HX-Reswap" "none"
      renderHtml $ do
        Login.emailTakenError True False
        Login.passwordConfirmationError True False
        button_
          [ class_ "btn btn-primary"
          , type_ "submit"
          , id_ "sign-up"
          , hxSwapOob "outerHTML"
          ]
          "Sign Up"
    else renderHtml do
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
