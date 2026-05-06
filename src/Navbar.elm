module Navbar exposing (navbar)

import Html exposing (Html, a, button, div, li, text, ul)
import Html.Attributes as Attr exposing (attribute, class, href, tabindex)
import Route exposing (Route(..), toUrl)
import Svg
import Svg.Attributes as SvgAttr


navbar : Route -> Html msg
navbar route =
    div [ class "navbar bg-base-100" ]
        [ div [ class "navbar-start" ]
            [ div [ class "dropdown" ]
                [ div
                    [ attribute "role" "button"
                    , tabindex 0
                    , class "btn btn-ghost lg:hidden"
                    ]
                    [ hamburgerIcon ]
                , ul [ class "menu menu-xl dropdown-content mt-3 z-1 p-2 shadow bg-base-100 rounded-box w-52" ]
                    (menuItems route)
                ]
            , a [ class "btn btn-ghost text-xl", href "/" ] [ text "budget" ]
            , div [ class "hidden lg:flex" ]
                [ ul [ class "menu menu-horizontal px-1" ]
                    (menuItems route)
                ]
            ]
        , div [ class "navbar-end" ]
            [ ul [ class "menu menu-horizontal px-1" ]
                [ li []
                    [ button [ class "btn btn-ghost" ] [ text "Log Out" ]
                    ]
                ]
            ]
        ]


hamburgerIcon : Html msg
hamburgerIcon =
    Svg.svg
        [ SvgAttr.class "h-6 w-6"
        , SvgAttr.fill "none"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.stroke "currentColor"
        ]
        [ Svg.path
            [ SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.d "M4 6h16M4 12h8m-8 6h16"
            ]
            []
        ]


menuItems : Route -> List (Html msg)
menuItems activeRoute =
    List.map (menuItem activeRoute)
        [ ( HomeR, "Home" )
        , ( DefinitionAdminR, "Definitions" )
        , ( ArchiveR, "Archive" )
        ]


menuItem : Route -> ( Route, String ) -> Html msg
menuItem activeRoute ( route, label ) =
    li [ Attr.classList [ ( "menu-active", activeRoute == route ) ] ]
        [ a [ href (toUrl route) ] [ text label ]
        ]
