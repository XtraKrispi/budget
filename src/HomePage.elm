module HomePage exposing (..)

import BusinessLogic exposing (computeResults, defaultScratch, extractItems, rawDefinitionDecoder, rawScratchDecoder)
import Date exposing (Date)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode
import Maybe.Extra as ME
import Numeral
import Ports.Clipboard exposing (copyToClipboard)
import Ports.Definitions exposing (fetchDefinitions, fetchDefinitionsFailure, fetchDefinitionsSuccess)
import Ports.Scratch exposing (fetchScratch, fetchScratchFailure, fetchScratchSuccess)
import RemoteData exposing (RemoteData)
import Task
import Types exposing (BudgetDefinition, Item, Scratch)


type Model
    = Initializing
    | Initialized HomePageModel


type alias HomePageModel =
    { definitions : RemoteData String (List BudgetDefinition)
    , scratch : RemoteData String Scratch
    , today : Date
    , editingScratch : Scratch
    }


type Msg
    = GotToday Date
    | CopyAmount Float
    | Skip Item
    | Pay Item
    | RecalculateTotals
    | RecalculateSuccess
    | RecalculateFailed
    | DefinitionsFetched (Result String (List BudgetDefinition))
    | ScratchFetched (Result String Scratch)


init : ( Model, Cmd Msg )
init =
    ( Initializing
    , Task.perform GotToday Date.today
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotToday date ->
            case model of
                Initializing ->
                    ( Initialized
                        { definitions = RemoteData.Loading
                        , scratch = RemoteData.Loading
                        , today = date
                        , editingScratch = defaultScratch date
                        }
                    , Cmd.batch [ fetchDefinitions (), fetchScratch () ]
                    )

                Initialized mdl ->
                    ( Initialized { mdl | today = date }, Cmd.none )

        CopyAmount amt ->
            ( model, copyToClipboard (String.fromFloat amt) )

        Skip item ->
            ( model, Cmd.none )

        Pay item ->
            ( model, Cmd.none )

        RecalculateTotals ->
            ( model, Cmd.none )

        RecalculateSuccess ->
            case model of
                Initialized mdl ->
                    ( Initialized { mdl | scratch = RemoteData.map (always mdl.editingScratch) mdl.scratch }, Cmd.none )

                Initializing ->
                    ( model, Cmd.none )

        RecalculateFailed ->
            ( model, Cmd.none )

        DefinitionsFetched results ->
            case model of
                Initialized mdl ->
                    ( Initialized { mdl | definitions = RemoteData.fromResult results }, Cmd.none )

                Initializing ->
                    ( model, Cmd.none )

        ScratchFetched scratch ->
            case model of
                Initialized mdl ->
                    ( Initialized
                        { mdl
                            | scratch =
                                case scratch of
                                    Ok s ->
                                        RemoteData.Success s

                                    Err _ ->
                                        RemoteData.Success (defaultScratch mdl.today)
                        }
                    , Cmd.none
                    )

                Initializing ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ fetchDefinitionsSuccess
            (\raw ->
                case
                    Decode.decodeValue (Decode.list rawDefinitionDecoder) raw
                of
                    Ok defs ->
                        DefinitionsFetched (Ok defs)

                    Err _ ->
                        DefinitionsFetched (Err "Couldn't convert from DB to app types")
            )
        , fetchDefinitionsFailure (\err -> DefinitionsFetched (Err err))
        , fetchScratchSuccess
            (\raw ->
                case raw of
                    Nothing ->
                        ScratchFetched (Err "No scratch found")

                    Just s ->
                        case Decode.decodeValue rawScratchDecoder s of
                            Ok scratch ->
                                ScratchFetched (Ok scratch)

                            Err _ ->
                                ScratchFetched (Err "Couldn't convert from DB to app types")
            )
        , fetchScratchFailure (\err -> DefinitionsFetched (Err err))
        ]


getItemsAndScratch : HomePageModel -> RemoteData String ( List Item, Scratch )
getItemsAndScratch model =
    RemoteData.map2
        (\defs s ->
            ( extractItems s.endDate defs, s )
        )
        model.definitions
        model.scratch


itemsSection : Model -> Html Msg
itemsSection mdl =
    case mdl of
        Initializing ->
            Html.div [] []

        Initialized model ->
            let
                content =
                    case RemoteData.map Tuple.first (getItemsAndScratch model) of
                        RemoteData.Success items ->
                            if List.isEmpty items then
                                [ Html.div [] [ Html.text "No items to display!" ] ]

                            else
                                items
                                    |> List.map renderItem

                        RemoteData.NotAsked ->
                            []

                        RemoteData.Loading ->
                            []

                        RemoteData.Failure _ ->
                            []
            in
            Html.node "main" [ Attr.class "flex flex-col space-y-4" ] content


formatAmount : Float -> String
formatAmount =
    Numeral.format "$0,0.00"


formatDate : Date -> String
formatDate =
    Date.format "Y-MM-dd"


renderItem : Item -> Html Msg
renderItem item =
    Html.div [ Attr.class "card w-96 bg-base-100 shadow-xl" ]
        [ Html.div [ Attr.class "card-body" ]
            [ Html.h2 [ Attr.class "card-title flex justify-between" ]
                [ Html.span [] [ Html.text item.definition.description ]
                , Html.span [ Attr.class "cursor-pointer", Events.onClick (CopyAmount item.definition.amount) ] [ item.definition.amount |> formatAmount |> Html.text ]
                ]
            , Html.p [] [ item.date |> formatDate |> Html.text ]
            , Html.div [ Attr.class "flex justify-between items-center" ]
                [ Html.div []
                    [ if item.definition.isAutomatic then
                        Html.div [ Attr.class "badge badge-primary" ] [ Html.text "Automatic" ]

                      else
                        Html.text ""
                    ]
                , Html.div [ Attr.class "card-actions justify-end" ]
                    [ Html.button
                        [ Attr.class "btn"
                        , Events.onClick (Skip item)
                        ]
                        [ Html.text "Skip" ]
                    , Html.button
                        [ Attr.class "btn btn-primary"
                        , Events.onClick (Pay item)
                        ]
                        [ Html.text "Pay" ]
                    ]
                ]
            ]
        ]


scratchArea : Model -> Html Msg
scratchArea mdl =
    case mdl of
        Initializing ->
            Html.aside [] []

        Initialized model ->
            case getItemsAndScratch model of
                RemoteData.Success ( items, scratch ) ->
                    let
                        results =
                            computeResults items scratch

                        amountValue amt =
                            if amt == 0 then
                                ""

                            else
                                Numeral.format "0.00" amt
                    in
                    Html.aside [ Attr.class "prose flex flex-col space-y-4 min-w-[320px]" ]
                        [ Html.form
                            [ Attr.class "flex flex-col space-y-4"
                            , Events.onSubmit RecalculateTotals
                            ]
                            [ Html.input
                                [ Attr.class "input input-bordered w-full max-w-xs"
                                , Attr.value (formatDate scratch.endDate)
                                , Attr.type_ "date"
                                ]
                                []
                            , Html.input
                                [ Attr.class "input input-bordered w-full max-w-xs"
                                , Attr.placeholder "Amount in account"
                                , Attr.type_ "number"
                                , Attr.value (amountValue scratch.amountAvailable)
                                ]
                                []
                            , Html.input
                                [ Attr.class "input input-bordered w-full max-w-xs"
                                , Attr.placeholder "Amount to be left over"
                                , Attr.type_ "number"
                                , Attr.value (amountValue scratch.amountLeftOver)
                                ]
                                []
                            , Html.button
                                [ Attr.class "btn btn-primary"
                                , Attr.type_ "submit"
                                ]
                                [ Html.text "Recalculate" ]
                            ]
                        , Html.div []
                            [ Html.h3 [ Attr.class "flex justify-between" ]
                                [ Html.span [] [ Html.text "Total Owing: " ]
                                , Html.span [] [ results.totalOwing |> formatAmount |> Html.text ]
                                ]
                            , Html.h3 [ Attr.class "flex justify-between" ]
                                [ Html.span [] [ Html.text "Total Outstanding: " ]
                                , Html.span [] [ results.totalOutstanding |> formatAmount |> Html.text ]
                                ]
                            ]
                        ]

                _ ->
                    Html.aside [] []


view : Model -> Html Msg
view model =
    Html.div [ Attr.class "px-20 py-10" ]
        [ Html.header [ Attr.class "prose lg:prose-xl flex space-x-4" ]
            [ Html.h2 [] [ Html.text "Home" ]
            ]
        , Html.div [ Attr.class "flex space-x-12 w-screen" ]
            [ itemsSection model
            , scratchArea model
            ]
        ]
