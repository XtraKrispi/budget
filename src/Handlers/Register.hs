module Handlers.Register where

import Db qualified
import Environment (HasAppEnvironment, HasDbPath)
import Handlers.Global (errorToast)
import Html.Common (addToast)
import Html.Login qualified as Login
import Htmx.Attributes
import Lucid
import Model
import Password qualified
import Relude
import Web.Scotty.Trans

postRegister :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => ActionT m ()
postRegister = do
  emailAddress <- formParam "register-email"
  password <- formParam "register-password"
  passwordConfirmation <- formParam "register-password-confirm"
  name <- formParam "register-name"
  eUser <- lift $ Db.getUserByEmail emailAddress
  case eUser of
    Left _ -> pure ()
    Right mUser -> do
      if isNothing mUser && password == passwordConfirmation
        then do
          hashed <- Password.hashPassword password
          results <- lift $ Db.insertUser (User emailAddress name hashed)
          case results of
            Left _ -> errorToast "There was an issue signing up, please try again."
            Right _ -> do
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

postRegisterValidate :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => ActionT m ()
postRegisterValidate = do
  isEmail <- (== Just "register-email") <$> header "HX-Trigger-Name"
  emailAddress <- formParam "register-email"
  password :: Password PlainText <- formParam "register-password"
  passwordConfirmation :: Password PlainText <- formParam "register-password-confirm"
  eUser <- lift $ Db.getUserByEmail emailAddress
  case eUser of
    Left _ -> pure ()
    Right mUser -> do
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