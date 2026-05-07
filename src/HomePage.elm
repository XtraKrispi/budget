module HomePage exposing (..)

import BusinessLogic exposing (computeResults, defaultScratch, encodeScratch, extractItems, rawDefinitionDecoder, rawScratchDecoder)
import Date exposing (Date, fromIsoString, toIsoString)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode
import Numeral
import Ports.Clipboard exposing (copyToClipboard)
import Ports.Definitions exposing (fetchDefinitions, fetchDefinitionsFailure, fetchDefinitionsSuccess)
import Ports.Scratch exposing (fetchScratch, fetchScratchFailure, fetchScratchSuccess, insertScratch, saveScratchFailure, saveScratchSuccess, updateScratch)
import RemoteData exposing (RemoteData)
import Task
import Toast exposing (Toast)
import Toasty
import Types exposing (BudgetDefinition, Item, Scratch, SessionInfo)


type Model
    = Initializing
    | Initialized HomePageModel


type alias EditingScratch =
    { endDate : String
    , amountInBank : String
    , amountLeftOver : String
    }


type alias HomePageModel =
    { definitions : RemoteData String (List BudgetDefinition)
    , scratch : RemoteData String (Maybe ( Scratch, Int ))
    , today : Date
    , editingScratch : EditingScratch
    , toasties : Toasty.Stack (Toast Msg)
    }


type Msg
    = GotToday Date
    | CopyAmount Float
    | Skip Item
    | Pay Item
    | RecalculateTotals
    | RecalculateSuccess Int
    | RecalculateFailed String
    | DefinitionsFetched (Result String (List BudgetDefinition))
    | ScratchFetched (Result String (Maybe ( Scratch, Int )))
    | ScratchDateUpdated String
    | ScratchAmountInBankUpdated String
    | ScratchAmountLeftOverUpdated String
    | ToastyMsg (Toasty.Msg (Toast Msg))


init : ( Model, Cmd Msg )
init =
    ( Initializing
    , Task.perform GotToday Date.today
    )


toEditingScratch : Scratch -> EditingScratch
toEditingScratch { endDate, amountInBank, amountLeftOver } =
    { endDate = toIsoString endDate
    , amountInBank = Numeral.format "0.00" amountInBank
    , amountLeftOver = Numeral.format "0.00" amountLeftOver
    }


toScratch : EditingScratch -> Maybe Scratch
toScratch { endDate, amountInBank, amountLeftOver } =
    Maybe.map3 Scratch
        (fromIsoString endDate |> Result.toMaybe)
        (String.toFloat amountInBank)
        (String.toFloat amountLeftOver)


update : SessionInfo -> Msg -> Model -> ( Model, Cmd Msg )
update sessionInfo msg model =
    case msg of
        GotToday date ->
            case model of
                Initializing ->
                    ( Initialized
                        { definitions = RemoteData.Loading
                        , scratch = RemoteData.Loading
                        , today = date
                        , editingScratch = toEditingScratch (defaultScratch date)
                        , toasties = Toasty.initialState
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
            case model of
                Initialized mdl ->
                    case ( toScratch mdl.editingScratch, mdl.scratch ) of
                        ( Just scratch, RemoteData.Success (Just ( _, id )) ) ->
                            ( model, updateScratch { data = encodeScratch scratch, id = id } )

                        ( Just scratch, _ ) ->
                            ( model, insertScratch { data = encodeScratch scratch, userId = sessionInfo.userId } )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RecalculateSuccess id ->
            case model of
                Initialized mdl ->
                    case toScratch mdl.editingScratch of
                        Just scratch ->
                            ( Initialized { mdl | scratch = RemoteData.map (always (Just ( scratch, id ))) mdl.scratch }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Initializing ->
                    ( model, Cmd.none )

        RecalculateFailed err ->
            ( model, Cmd.none )

        DefinitionsFetched results ->
            case model of
                Initialized mdl ->
                    ( { mdl | definitions = RemoteData.fromResult results }, Cmd.none )
                        |> Tuple.mapFirst Initialized

                Initializing ->
                    ( model, Cmd.none )

        ScratchFetched scratch ->
            case model of
                Initialized mdl ->
                    let
                        ( newScratch, editingScratch ) =
                            case scratch of
                                Ok Nothing ->
                                    ( RemoteData.Success Nothing, defaultScratch mdl.today )

                                Ok (Just s) ->
                                    ( RemoteData.Success (Just s), Tuple.first s )

                                Err err ->
                                    ( RemoteData.Failure err, defaultScratch mdl.today )
                    in
                    ( Initialized
                        { mdl
                            | scratch = newScratch
                            , editingScratch = toEditingScratch editingScratch
                        }
                    , Cmd.none
                    )

                Initializing ->
                    ( model, Cmd.none )

        ScratchDateUpdated str ->
            case model of
                Initialized mdl ->
                    let
                        editingScratch =
                            mdl.editingScratch
                    in
                    ( Initialized
                        { mdl
                            | editingScratch =
                                { editingScratch
                                    | endDate = str
                                }
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ScratchAmountInBankUpdated str ->
            case model of
                Initialized mdl ->
                    let
                        editingScratch =
                            mdl.editingScratch
                    in
                    ( Initialized
                        { mdl
                            | editingScratch =
                                { editingScratch
                                    | amountInBank = str
                                }
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ScratchAmountLeftOverUpdated str ->
            case model of
                Initialized mdl ->
                    let
                        editingScratch =
                            mdl.editingScratch
                    in
                    ( Initialized
                        { mdl
                            | editingScratch =
                                { editingScratch
                                    | amountLeftOver = str
                                }
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ToastyMsg subMsg ->
            case model of
                Initialized mdl ->
                    let
                        ( newMdl, newCmd ) =
                            Toasty.update Toast.toastyConfig ToastyMsg subMsg mdl
                    in
                    ( Initialized newMdl, newCmd )

                _ ->
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
                        ScratchFetched (Ok Nothing)

                    Just s ->
                        case Decode.decodeValue rawScratchDecoder s of
                            Ok val ->
                                ScratchFetched (Ok (Just val))

                            Err _ ->
                                ScratchFetched (Err "Couldn't convert from DB to app types")
            )
        , fetchScratchFailure (ScratchFetched << Err)
        , saveScratchSuccess RecalculateSuccess
        , saveScratchFailure RecalculateFailed
        ]


getItemsAndScratch : HomePageModel -> RemoteData String ( List Item, Scratch )
getItemsAndScratch model =
    RemoteData.map2
        (\defs s ->
            case s of
                Nothing ->
                    let
                        def =
                            defaultScratch model.today
                    in
                    ( extractItems def.endDate defs, def )

                Just ( s_, _ ) ->
                    ( extractItems s_.endDate defs, s_ )
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
                    in
                    Html.aside [ Attr.class "prose flex flex-col space-y-4 min-w-[320px]" ]
                        [ Html.form
                            [ Attr.class "flex flex-col space-y-4"
                            , Events.onSubmit RecalculateTotals
                            ]
                            [ Html.input
                                [ Attr.class "input input-bordered w-full max-w-xs"
                                , Attr.type_ "date"
                                , Attr.value model.editingScratch.endDate
                                , Events.onInput ScratchDateUpdated
                                ]
                                []
                            , Html.input
                                [ Attr.class "input input-bordered w-full max-w-xs"
                                , Attr.placeholder "Amount in account"
                                , Attr.value model.editingScratch.amountInBank
                                , Events.onInput ScratchAmountInBankUpdated
                                ]
                                []
                            , Html.input
                                [ Attr.class "input input-bordered w-full max-w-xs"
                                , Attr.placeholder "Amount to be left over"
                                , Attr.value model.editingScratch.amountLeftOver
                                , Events.onInput ScratchAmountLeftOverUpdated
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
        , Html.div [ Attr.class "toast toast-top toast-center" ]
            [ case model of
                Initialized mdl ->
                    Toasty.view Toast.toastyConfig Toast.renderToast ToastyMsg mdl.toasties

                _ ->
                    Html.text ""
            ]
        ]
