module Handlers.Register where

import AppError (AppError)
import Control.Monad (when)
import Data.Maybe (isJust, isNothing)
import Effectful
import Effectful.Error.Static (Error)
import Effects.HashPassword
import Effects.UserStore
import Handlers.Model
import Handlers.Utils
import Html.Common (addToast)
import Html.Login qualified as Login
import Htmx.Attributes
import Lucid
import Model

postRegister ::
  ( UserStore :> es
  , HashPassword :> es
  , Error AppError :> es
  ) =>
  Request ->
  Eff es Response
postRegister request = do
  emailAddress <- getParam request "register-email"
  password :: Password PlainText <- getParam request "register-password"
  passwordConfirmation <- getParam request "register-password-confirm"
  name <- getParam request "register-name"
  mUser <- Effects.UserStore.get emailAddress
  if isNothing mUser && password == passwordConfirmation
    then do
      hashed <- hashPassword password
      Effects.UserStore.insert (User emailAddress name hashed)
      pure $ makeResponse [("HX-Trigger", "hideRegistrationModal")] [] $ addToast Success (span_ "You've been signed up! Please log in below")
    else pure $ htmlResponse do
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
  , Error AppError :> es
  ) =>
  Request ->
  Eff es Response
postRegisterValidate request = do
  let isEmail = getHeader request "HX-Trigger-Name" == Just "register-email"
  emailAddress <- getParam request "register-email"
  password :: Password PlainText <- getParam request "register-password"
  passwordConfirmation <- getParam request "register-password-confirm"
  mUser <- Effects.UserStore.get emailAddress
  let isValid = isNothing mUser && password == passwordConfirmation
  if isValid
    then do
      pure $ makeResponse [("HX-Reswap", "none")] [] $ do
        Login.emailTakenError True False
        Login.passwordConfirmationError True False
        button_
          [ class_ "btn btn-primary"
          , type_ "submit"
          , id_ "sign-up"
          , hxSwapOob "outerHTML"
          ]
          "Sign Up"
    else pure $ htmlResponse do
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
