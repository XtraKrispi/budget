module View.Common exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Svg
import Svg.Attributes as SA


dialogCloseButton : Html msg
dialogCloseButton =
    Html.form [ Attr.method "dialog" ]
        [ Html.button [ Attr.class "btn btn-sm btn-circle btn-ghost absolute right-2 top-2" ]
            [ Svg.svg
                [ SA.class "h-6 w-6"
                , SA.fill "none"
                , SA.viewBox "0 0 24 24"
                ]
                [ Svg.path
                    [ SA.stroke "currentColor"
                    , SA.strokeLinecap "round"
                    , SA.strokeLinejoin "round"
                    , SA.strokeWidth "2"
                    , SA.d "M6 18 17.94 6M18 18 6.06 6"
                    ]
                    []
                ]
            ]
        ]
