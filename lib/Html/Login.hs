{-# LANGUAGE QuasiQuotes #-}

module Html.Login where

import Html.Common (classList, withPageShell)
import Html.Dialog qualified as Dialog
import Htmx.Attributes
import Lucid
import Text.RawString.QQ

fullPage :: Html ()
fullPage = withPageShell do
  div_ [class_ "bg-base-200 min-h-screen min-w-screen flex flex-col"] do
    div_ [class_ "w-screen bg-base-400 min-h-[200px] flex justify-center items-center"] do
      h1_ [class_ "text-8xl font-bold"] "Budget"
    div_
      [ class_ "hero grow h-full"
      ]
      do
        div_ [class_ "hero-content flex-col lg:flex-row-reverse"] do
          dialog_ [class_ "modal", id_ "registration-modal"] do
            div_ [class_ "modal-box"] do
              Dialog.closeButton
              h3_ [class_ "font-bold text-lg"] "Sign Up"
              registrationForm
            form_ [method_ "dialog", class_ "modal-backdrop"] do
              button_ "close"
          div_ [class_ "text-center lg:text-left"] do
            h1_ [class_ "text-5xl font-bold"] "Login now!"
            p_ [class_ "pt-6"] "Please log in to the Budget system to see what you have upcoming and to make any changes!"
            p_ [class_ "py-2"] "Don't have an account? No problem! Sign up here!"
            button_
              [ class_ "btn btn-primary"
              , hyper_
                  [r|on click call #registration-modal-form.reset()
                              call #registration-modal.showModal() end|]
              ]
              -- call #registration-modal-form.reset()
              "Sign Up"
          div_
            [class_ "card bg-base-100 w-full max-w-sm shrink-0 shadow-2xl"]
            do
              form_ [class_ "card-body", hxPost "/login"] do
                div_ [class_ "form-control"] do
                  label_ [class_ "label"] do
                    span_ [class_ "label-text"] "Email"
                  input_
                    [ class_ "input input-bordered"
                    , type_ "email"
                    , placeholder_ "email"
                    , name_ "email"
                    , required_ "required"
                    , autofocus_
                    ]
                div_ [class_ "form-control"] do
                  label_ [class_ "label"] do
                    span_ [class_ "label-text"] "Password"
                  input_
                    [ type_ "password"
                    , placeholder_ "password"
                    , class_ "input input-bordered"
                    , name_ "password"
                    , required_ "required"
                    ]
                  label_ [class_ "label", hxBoost] do
                    a_ [class_ "label-text-alt link link-hover", href_ "/reset-password"] "Forgot password?"
                div_ [class_ "form-control mt-6"] do
                  button_ [class_ "btn btn-primary"] "Login"

registrationForm :: Html ()
registrationForm = do
  form_
    [ class_ "flex flex-col space-y-2"
    , hxPost "/register"
    , hxSwap "outerHTML"
    , id_ "registration-modal-form"
    ]
    do
      label_ [class_ "form-control w-full max-w-xs mt-2"] do
        input_
          [ class_ "input input-bordered w-full max-w-xs grow"
          , placeholder_ "Email"
          , name_ "register-email"
          , id_ "register-email"
          , required_ "required"
          , type_ "email"
          , hxTrigger "keyup delay:500ms changed"
          , hxPost "/register/validate"
          , hxTarget "#email-taken-error"
          , hxSwap "outerHTML"
          , autocomplete_ "new-password"
          ]
        emailTakenError False False
      label_ [class_ "form-control w-full max-w-xs mt-2"] do
        input_
          [ class_ "input input-bordered w-full max-w-xs"
          , placeholder_ "Name"
          , name_ "register-name"
          , id_ "register-name"
          , required_ "required"
          , type_ "text"
          , hxValidate
          , autocomplete_ "new-password"
          ]
      div_ do
        label_ [class_ "form-control w-full max-w-xs"] do
          input_
            [ class_ "input input-bordered w-full max-w-xs"
            , placeholder_ "Password"
            , name_ "register-password"
            , id_ "register-password"
            , required_ "required"
            , type_ "password"
            , id_ "password"
            , hxTrigger "keyup delay:500ms changed"
            , hxPost "/register/validate"
            , hxTarget "#user-validation-password-match"
            , hxSwap "outerHTML"
            , autocomplete_ "new-password"
            ]
        label_ [class_ "form-control w-full max-w-xs mt-2"] do
          input_
            [ class_ "input input-bordered w-full max-w-xs"
            , placeholder_ "Password Confirmation"
            , name_ "register-password-confirm"
            , id_ "register-password-confirm"
            , required_ "required"
            , type_ "password"
            , id_ "password-confirm"
            , hxTrigger "keyup delay:500ms changed"
            , hxPost "/register/validate"
            , hxTarget "#user-validation-password-match"
            , hxSwap "outerHTML"
            , autocomplete_ "new-password"
            ]
          passwordConfirmationError False False
      div_ [class_ "modal-action"] do
        button_
          [ class_ "btn btn-primary"
          , type_ "submit"
          , id_ "sign-up"
          ]
          "Sign Up"

emailTakenError :: Bool -> Bool -> Html ()
emailTakenError oob visible =
  div_
    ( [ classList [("label", True), ("hidden", not visible)]
      , id_ "email-taken-error"
      ]
        ++ ([hxSwapOob "outerHTML" | oob])
    )
    do
      span_ [class_ "label-text-alt text-error"] "Email is already in use"

passwordConfirmationError :: Bool -> Bool -> Html ()
passwordConfirmationError oob visible =
  div_
    ( [ classList
          [ ("label", True)
          , ("hidden", not visible)
          ]
      , id_ "user-validation-password-match"
      ]
        ++ ([hxSwapOob "outerHTML" | oob])
    )
    do
      span_ [class_ "label-text-alt text-error"] "Passwords do not match"
