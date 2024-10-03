module Html.Dialog where

import Lucid
import Svg

closeButton :: Html ()
closeButton = form_ [method_ "dialog"] do
  button_ [class_ "btn btn-sm btn-circle btn-ghost absolute right-2 top-2"] do
    svg_ [class_ "h-6 w-6", fill_ "none", viewBox_ "0 0 24 24"] do
      path_
        [ stroke_ "currentColor"
        , strokeLinecap_ "round"
        , strokeLinejoin_ "round"
        , strokeWidth_ "2"
        , d_ "M6 18 17.94 6M18 18 6.06 6"
        ]
        ""