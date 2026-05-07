module Toast exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Svg
import Svg.Attributes as SvgAttr
import Toasty


type Severity
    = Info
    | Success
    | Warning
    | Error


type ToastType
    = Persistent
    | Duration Int


type alias Toast msg =
    { severity : Severity
    , message : Html msg
    }


toastyConfig : Toasty.Config msg
toastyConfig =
    Toasty.config
        |> Toasty.transitionOutDuration 150
        |> Toasty.transitionOutAttrs [ Attr.class "opacity-0" ]
        |> Toasty.itemAttrs [ Attr.class "transition-all duration-150" ]
        |> Toasty.delay 3000


severityIcon : Severity -> Html msg
severityIcon severity =
    case severity of
        Info ->
            Svg.svg
                [ SvgAttr.fill "none"
                , SvgAttr.viewBox "0 0 24 24"
                , SvgAttr.class "h-6 w-6 shrink-0 stroke-current"
                ]
                [ Svg.path
                    [ SvgAttr.strokeLinecap "round"
                    , SvgAttr.strokeLinejoin "round"
                    , SvgAttr.strokeWidth "2"
                    , SvgAttr.d "M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
                    ]
                    []
                ]

        Success ->
            Svg.svg
                [ SvgAttr.fill "none"
                , SvgAttr.viewBox "0 0 24 24"
                , SvgAttr.class "h-6 w-6 shrink-0 stroke-current"
                ]
                [ Svg.path
                    [ SvgAttr.strokeLinecap "round"
                    , SvgAttr.strokeLinejoin "round"
                    , SvgAttr.strokeWidth "2"
                    , SvgAttr.d "M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"
                    ]
                    []
                ]

        Error ->
            Svg.svg
                [ SvgAttr.fill "none"
                , SvgAttr.viewBox "0 0 24 24"
                , SvgAttr.class "h-6 w-6 shrink-0 stroke-current"
                ]
                [ Svg.path
                    [ SvgAttr.strokeLinecap "round"
                    , SvgAttr.strokeLinejoin "round"
                    , SvgAttr.strokeWidth "2"
                    , SvgAttr.d "M10 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2m7-2a9 9 0 11-18 0 9 9 0 0118 0z"
                    ]
                    []
                ]

        Warning ->
            Svg.svg
                [ SvgAttr.fill "none"
                , SvgAttr.viewBox "0 0 24 24"
                , SvgAttr.class "h-6 w-6 shrink-0 stroke-current"
                ]
                [ Svg.path
                    [ SvgAttr.strokeLinecap "round"
                    , SvgAttr.strokeLinejoin "round"
                    , SvgAttr.strokeWidth "2"
                    , SvgAttr.d "M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"
                    ]
                    []
                ]


renderToast : Toast msg -> Html msg
renderToast toast =
    Html.div
        [ Attr.class "alert"
        , Attr.classList
            [ ( "alert-info", toast.severity == Info )
            , ( "alert-success", toast.severity == Success )
            , ( "alert-error", toast.severity == Error )
            , ( "alert-warning", toast.severity == Warning )
            ]
        ]
        [ severityIcon toast.severity, toast.message ]
