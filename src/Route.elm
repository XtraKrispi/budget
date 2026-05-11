module Route exposing (..)

import Url.Parser exposing ((</>), Parser, map, oneOf, s, top)


type Route
    = HomeR
    | LoginR
    | DefinitionAdminR
    | ArchiveR
    | NotFoundR


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map LoginR (s "login")
        , map ArchiveR (s "archive")
        , map DefinitionAdminR (s "admin" </> s "definitions")
        , map HomeR top
        ]


toUrl : Route -> String
toUrl route =
    "#"
        ++ (case route of
                HomeR ->
                    "/"

                LoginR ->
                    "/login"

                DefinitionAdminR ->
                    "/admin/definitions"

                ArchiveR ->
                    "/archive"

                NotFoundR ->
                    "/not-found"
           )
