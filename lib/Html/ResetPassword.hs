module Html.ResetPassword where

import Data.Text (Text)
import Html.Common (classList, withPageShell)
import Htmx.Attributes (hxPost, hxSwap, hxSwapOob, hxTarget, hxTrigger, hxValidate)
import Lucid
import Model.Common
import Model.Token
import Network.URI.Encode (encodeText)

landingPage :: Html ()
landingPage = withPageShell do
  div_ [class_ "bg-base-200 hero min-h-screen min-w-screen flex flex-col justify-center"] do
    div_ [class_ "hero-content flex-col lg:flex-row-reverse"] do
      div_ [class_ "text-center lg:text-left"] do
        h1_ [class_ "text-5xl font-bold"] "Reset Password"
        p_ [class_ "py-6"] "Please enter the email you signed up with"
      div_ [class_ "card bg-base-100 w-full max-w-sm shrink-0 shadow-2xl"] do
        form_ [class_ "card-body", hxPost "/reset-password"] do
          div_ [class_ "form-control"] do
            label_ [class_ "label"] do
              span_ [class_ "label-text"] "Email"
            input_
              [ class_ "input input-bordered"
              , type_ "email"
              , placeholder_ "email"
              , required_ "required"
              , name_ "email"
              , hxValidate
              ]
          div_ [class_ "form-control mt-6"] do
            button_ [class_ "btn btn-primary"] do
              span_ [class_ "htmx-indicator loading loading-spinner"] ""
              span_ "Reset Password"

tokenPage :: Token PlainText -> Html ()
tokenPage (Token token) = withPageShell do
  div_ [class_ "bg-base-200 hero min-h-screen min-w-screen flex flex-col justify-center"] do
    div_ [class_ "hero-content flex-col lg:flex-row-reverse"] do
      div_ [class_ "text-center lg:text-left"] do
        h1_ [class_ "text-5xl font-bold"] "Reset Password"
        p_ [class_ "py-6"] "Please enter your new password"
      div_ [class_ "card bg-base-100 w-full max-w-sm shrink-0 shadow-2xl"] do
        form_ [class_ "card-body", hxPost ("/reset-password/" <> encodeText token), hxSwap "outerHTML"] do
          div_ [class_ "form-control"] do
            label_ [class_ "label"] do
              span_ [class_ "label-text"] "Password"
            input_
              [ type_ "password"
              , placeholder_ "password"
              , class_ "input input-bordered"
              , required_ "required"
              , name_ "password"
              , hxTrigger "keyup delay:300ms changed"
              , hxPost "/reset-password/validate"
              , hxTarget "#change-password"
              , hxSwap "outerHTML"
              , autocomplete_ "new-password"
              ]
          div_ [class_ "form-control"] do
            label_ [class_ "label"] do
              span_ [class_ "label-text"] "Confirm Password"
            input_
              [ type_ "password"
              , placeholder_ "password"
              , class_ "input input-bordered"
              , required_ "required"
              , name_ "password-confirmation"
              , hxTrigger "keyup delay:300ms changed"
              , hxPost "/reset-password/validate"
              , hxTarget "#change-password"
              , hxSwap "outerHTML"
              , autocomplete_ "new-password"
              ]
            passwordConfirmationError False False
          div_ [class_ "form-control mt-6"] do
            button_
              [ class_ "btn btn-primary"
              , id_ "change-password"
              , type_ "submit"
              ]
              "Reset Password"

errorPage :: Html ()
errorPage = withPageShell do
  div_ [class_ "bg-base-200 hero min-h-screen min-w-screen flex flex-col justify-center"] do
    div_ [class_ "hero-content flex-col lg:flex-row-reverse"] do
      div_ [class_ "text-center lg:text-left"] do
        h1_ [class_ "text-5xl font-bold"] "Reset Password"
        p_ [class_ "py-6"] "Something went wrong. Please try again."
        p_ "Please note that your link will only work for 10 minutes."

resetPasswordEmail :: Text -> Html ()
resetPasswordEmail url = do
  div_ do
    h1_ "Budget - Reset Password"
    p_ "Please click the link below to reset your password."
    p_ "The link will expire in 10 minutes."
    p_ do
      a_ [href_ url] "Click here"

passwordConfirmationError :: Bool -> Bool -> Html ()
passwordConfirmationError oob visible =
  div_
    ( [ classList
          [ ("label", True)
          , ("hidden", not visible)
          ]
      , id_ "validation-password-match"
      ]
        ++ ([hxSwapOob "outerHTML" | oob])
    )
    do
      span_ [class_ "label-text-alt text-error"] "Passwords do not match"
