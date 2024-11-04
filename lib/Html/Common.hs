module Html.Common (withPageShell, withLayout, addToast, classList, AlertType (..)) where

import Data.Text (Text)
import Data.Text qualified as T
import Htmx.Attributes
import Lucid
import Model.User (User)
import Svg (d_, fill_, path_, strokeLinecap_, strokeLinejoin_, strokeWidth_, stroke_, viewBox_)

withPageShell :: Html () -> Html ()
withPageShell content =
  doctypehtml_ do
    head_ do
      title_ "Budget"
      link_ [rel_ "stylesheet", href_ "output.css"]
      script_ [src_ "https://unpkg.com/htmx.org@2.0.2"] ("" :: Text)
      script_ [src_ "https://unpkg.com/hyperscript.org@0.9.12"] ("" :: Text)
    body_ [class_ "w-screen"] do
      div_ [class_ "toast toast-top toast-end z-[1000]", id_ "toast-container"] ""
      content

withLayout :: User -> Html () -> Html ()
withLayout _ content =
  withPageShell do
    navbar
    content

navbar :: Html ()
navbar = div_ [class_ "navbar bg-base-100", hxBoost] do
  div_ [class_ "navbar-start"] do
    div_ [class_ "dropdown"] do
      div_
        [ role_ "button"
        , tabindex_ "0"
        , class_ "btn btn-ghost lg:hidden"
        ]
        do
          svg_
            [ class_ "h-6 w-6"
            , fill_ "none"
            , viewBox_ "0 0 24 24"
            , stroke_ "currentColor"
            ]
            do
              path_
                [ strokeLinecap_ "round"
                , strokeLinejoin_ "round"
                , strokeWidth_ "2"
                , d_ "M4 6h16M4 12h8m-8 6h16"
                ]
                ""
      ul_ [class_ "menu menu-sm dropdown-content mt-3 z-[1] p-2 shadow bg-base-100 rounded-box w-52"] do
        menuItems
    a_ [class_ "btn btn-ghost text-xl", href_ "/"] "budget"
    div_ [class_ "hidden lg:flex"] do
      ul_ [class_ "menu menu-horizontal px-1"] do
        menuItems
  div_ [class_ "navbar-end"] do
    ul_ [class_ "menu menu-horizontal px-1"] do
      li_ do
        a_ [hxDelete "/session"] "Log Out"
 where
  menuItems =
    mconcat $
      menuItem
        <$> [ ("/", "Home")
            , ("/admin/definitions", "Definitions")
            , ("/archive", "Archive")
            ]
  menuItem :: (Text, Text) -> Html ()
  menuItem (url, text) =
    li_ [] do
      a_ [href_ url] do
        toHtml text

data AlertType
  = Info
  | Success
  | Warning
  | Error
  deriving (Eq)

addToast :: AlertType -> Html () -> Html ()
addToast alertType content = do
  let iconPath = case alertType of
        Info -> "M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
        Success -> "M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"
        Warning -> "M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"
        Error -> "M10 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2m7-2a9 9 0 11-18 0 9 9 0 0118 0z"
  div_
    [ hxSwapOob "beforeend"
    , id_ "toast-container"
    ]
    do
      div_
        [ hxGet "/toast/clear"
        , hxSwap "outerHTML"
        , hxTrigger "load delay:3s"
        , classList
            [ ("alert", True)
            , ("alert-info", alertType == Info)
            , ("alert-success", alertType == Success)
            , ("alert-warning", alertType == Warning)
            , ("alert-error", alertType == Error)
            ]
        ]
        do
          svg_
            [ fill_ "none"
            , viewBox_ "0 0 24 24"
            , class_ "h-6 w-6 shrink-0 stroke-current"
            ]
            do
              path_
                [ strokeLinecap_ "round"
                , strokeLinejoin_ "round"
                , strokeWidth_ "2"
                , d_ iconPath
                ]
                ""

          content

classList :: [(Text, Bool)] -> Attributes
classList cs =
  class_ $ T.unwords (fst <$> filter snd cs)
