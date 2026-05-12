module ArchivePage exposing (..)

import BusinessLogic exposing (archiveDecoder, formatAmount, formatDate)
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode
import Ports.Archive exposing (fetchArchive, fetchArchiveFailure, fetchArchiveSuccess)
import RemoteData exposing (RemoteData)
import Types exposing (Archive, ArchiveAction(..))


type alias Model =
    { archive : RemoteData String (List Archive)
    }


type Msg
    = ArchiveFetched (Result String (List Archive))


init : ( Model, Cmd Msg )
init =
    ( { archive = RemoteData.Loading }, fetchArchive { includeDeletedDefinitions = True } )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ArchiveFetched results ->
            ( { model | archive = RemoteData.fromResult results }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ fetchArchiveSuccess
            (\raw ->
                case Decode.decodeValue (Decode.list archiveDecoder) raw of
                    Ok archive ->
                        ArchiveFetched (Ok archive)

                    Err _ ->
                        ArchiveFetched (Err "Couldn't convert from DB to app types")
            )
        , fetchArchiveFailure (ArchiveFetched << Err)
        ]


view : Model -> Html Msg
view model =
    Html.div [ Attr.class "px-20 py-10" ]
        [ Html.header [ Attr.class "prose lg:prose-xl flex space-x-4" ]
            [ Html.h2 [] [ Html.text "Archive" ] ]
        , Html.node "main"
            [ Attr.class "prose flex flex-col space-y-4" ]
            (case model.archive of
                RemoteData.Loading ->
                    [ Html.text "Loading" ]

                RemoteData.Success archive ->
                    if List.isEmpty archive then
                        [ Html.h3 [] [ Html.text "No items found..." ] ]

                    else
                        List.map renderArchiveItem archive

                _ ->
                    [ Html.text "" ]
            )
        ]


renderArchiveItem : Archive -> Html Msg
renderArchiveItem { description, amount, date, action } =
    Html.div [ Attr.class "card w-96 bg-base-100 shadow-xl" ]
        [ Html.div [ Attr.class "card-body" ]
            [ Html.h2 [ Attr.class "card-title flex justify-between" ]
                [ Html.span [] [ Html.text description ]
                , Html.span []
                    [ amount
                        |> formatAmount
                        |> Html.text
                    ]
                ]
            , Html.p [] [ date |> formatDate |> Html.text ]
            , Html.div [ Attr.class "flex justify-between items-center" ]
                [ Html.div []
                    [ case action of
                        PayAction ->
                            Html.div [ Attr.class "badge badge-primary" ] [ Html.text "Paid" ]

                        SkipAction ->
                            Html.div [ Attr.class "badge badge-accent" ] [ Html.text "Skipped" ]
                    ]
                ]
            ]
        ]
