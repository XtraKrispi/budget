module HomePage exposing (..)

import BusinessLogic exposing (archiveDecoder, computeResults, defaultScratch, definitionDecoder, encodeArchive, encodeScratch, extractItems, formatAmount, formatDate, isError, scratchDecoder)
import Date exposing (Date, fromIsoString, toIsoString)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode
import Numeral
import Ports.Archive exposing (fetchArchive, fetchArchiveFailure, fetchArchiveSuccess, insertArchive, insertArchiveFailure, insertArchiveSuccess)
import Ports.Clipboard exposing (copyToClipboard)
import Ports.Definitions exposing (fetchDefinitions, fetchDefinitionsFailure, fetchDefinitionsSuccess)
import Ports.Scratch exposing (fetchScratch, fetchScratchFailure, fetchScratchSuccess, insertScratch, saveScratchFailure, saveScratchSuccess, updateScratch)
import RemoteData exposing (RemoteData)
import Toast exposing (Toast)
import Toasty
import Types exposing (Archive, ArchiveAction(..), Definition, Item, Scratch, SessionInfo)


type alias Model =
    { definitions : RemoteData String (List ( Definition, Int ))
    , scratch : RemoteData String (Maybe ( Scratch, Int ))
    , archive : RemoteData String (List Archive)
    , today : Date
    , editingScratch : EditingScratch
    , toasties : Toasty.Stack (Toast Msg)
    }


type alias EditingScratch =
    { endDate : String
    , amountInBank : String
    , amountLeftOver : String
    }


type Msg
    = CopyAmount Float
    | Skip Item
    | Pay Item
    | RecalculateTotals
    | RecalculateSuccess Int
    | RecalculateFailed String
    | DefinitionsFetched (Result String (List ( Definition, Int )))
    | ScratchFetched (Result String (Maybe ( Scratch, Int )))
    | ArchiveFetched (Result String (List Archive))
    | ScratchDateUpdated String
    | ScratchAmountInBankUpdated String
    | ScratchAmountLeftOverUpdated String
    | ToastyMsg (Toasty.Msg (Toast Msg))
    | InsertArchiveSuccess Archive
    | InsertArchiveFailed String


init : Date -> ( Model, Cmd Msg )
init today =
    ( { definitions = RemoteData.Loading
      , scratch = RemoteData.Loading
      , archive = RemoteData.Loading
      , today = today
      , editingScratch = toEditingScratch (defaultScratch today)
      , toasties = Toasty.initialState
      }
    , Cmd.batch
        [ fetchDefinitions { includeDeleted = False }
        , fetchScratch ()
        , fetchArchive { includeDeletedDefinitions = False }
        ]
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
        CopyAmount amt ->
            ( model, copyToClipboard (String.fromFloat amt) )

        Skip item ->
            ( model, insertArchive { data = encodeArchive item SkipAction, userId = sessionInfo.userId } )

        Pay item ->
            ( model, insertArchive { data = encodeArchive item PayAction, userId = sessionInfo.userId } )

        RecalculateTotals ->
            case ( toScratch model.editingScratch, model.scratch ) of
                ( Just scratch, RemoteData.Success (Just ( _, id )) ) ->
                    ( model, updateScratch { data = encodeScratch scratch, id = id } )

                ( Just scratch, _ ) ->
                    ( model, insertScratch { data = encodeScratch scratch, userId = sessionInfo.userId } )

                _ ->
                    ( model, Cmd.none )

        RecalculateSuccess id ->
            case toScratch model.editingScratch of
                Just scratch ->
                    ( { model | scratch = RemoteData.map (always (Just ( scratch, id ))) model.scratch }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        RecalculateFailed _ ->
            ( model, Cmd.none )
                |> Toasty.addToast Toast.toastyConfig ToastyMsg { severity = Toast.Error, message = Html.span [] [ Html.text "There was an issue recalculating, please try again." ] }

        DefinitionsFetched results ->
            ( { model | definitions = RemoteData.fromResult results }, Cmd.none )
                |> Toasty.addToastIf Toast.toastyConfig ToastyMsg (\_ -> isError results) { severity = Toast.Error, message = Html.span [] [ Html.text "There was an error fetching results. Please refresh the page." ] }

        ScratchFetched scratch ->
            let
                ( newScratch, editingScratch ) =
                    case scratch of
                        Ok Nothing ->
                            ( RemoteData.Success Nothing, defaultScratch model.today )

                        Ok (Just s) ->
                            ( RemoteData.Success (Just s), Tuple.first s )

                        Err err ->
                            ( RemoteData.Failure err, defaultScratch model.today )
            in
            ( { model
                | scratch = newScratch
                , editingScratch = toEditingScratch editingScratch
              }
            , Cmd.none
            )
                |> Toasty.addToastIf Toast.toastyConfig ToastyMsg (\_ -> RemoteData.isFailure newScratch) { severity = Toast.Error, message = Html.span [] [ Html.text "There was an error fetching results. Please refresh the page." ] }

        ArchiveFetched results ->
            ( { model | archive = RemoteData.fromResult results }, Cmd.none )
                |> Toasty.addToastIf Toast.toastyConfig ToastyMsg (\_ -> isError results) { severity = Toast.Error, message = Html.span [] [ Html.text "There was an error fetching results. Please refresh the page." ] }

        ScratchDateUpdated str ->
            let
                editingScratch =
                    model.editingScratch
            in
            ( { model
                | editingScratch =
                    { editingScratch
                        | endDate = str
                    }
              }
            , Cmd.none
            )

        ScratchAmountInBankUpdated str ->
            let
                editingScratch =
                    model.editingScratch
            in
            ( { model
                | editingScratch =
                    { editingScratch
                        | amountInBank = str
                    }
              }
            , Cmd.none
            )

        ScratchAmountLeftOverUpdated str ->
            let
                editingScratch =
                    model.editingScratch
            in
            ( { model
                | editingScratch =
                    { editingScratch
                        | amountLeftOver = str
                    }
              }
            , Cmd.none
            )

        InsertArchiveSuccess archive ->
            ( { model | archive = RemoteData.map (\a -> archive :: a) model.archive }, Cmd.none )

        InsertArchiveFailed _ ->
            ( model, Cmd.none )
                |> Toasty.addToast Toast.toastyConfig ToastyMsg { severity = Toast.Error, message = Html.span [] [ Html.text "There was a problem executing the action. Please try again." ] }

        ToastyMsg subMsg ->
            Toasty.update Toast.toastyConfig ToastyMsg subMsg model


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ fetchDefinitionsSuccess
            (\raw ->
                case
                    Decode.decodeValue (Decode.list definitionDecoder) raw
                of
                    Ok defs ->
                        DefinitionsFetched (Ok defs)

                    Err _ ->
                        DefinitionsFetched (Err "Couldn't convert from DB to app types")
            )
        , fetchDefinitionsFailure (DefinitionsFetched << Err)
        , fetchScratchSuccess
            (\raw ->
                case raw of
                    Nothing ->
                        ScratchFetched (Ok Nothing)

                    Just s ->
                        case Decode.decodeValue scratchDecoder s of
                            Ok val ->
                                ScratchFetched (Ok (Just val))

                            Err _ ->
                                ScratchFetched (Err "Couldn't convert from DB to app types")
            )
        , fetchScratchFailure (ScratchFetched << Err)
        , saveScratchSuccess RecalculateSuccess
        , saveScratchFailure RecalculateFailed
        , insertArchiveSuccess
            (\raw ->
                case Decode.decodeValue archiveDecoder raw of
                    Ok archive ->
                        InsertArchiveSuccess archive

                    Err _ ->
                        InsertArchiveFailed "Failed to decode archive"
            )
        , insertArchiveFailure InsertArchiveFailed
        , fetchArchiveSuccess
            (\raw ->
                case Decode.decodeValue (Decode.list archiveDecoder) raw of
                    Ok archive ->
                        ArchiveFetched (Ok archive)

                    Err _ ->
                        ArchiveFetched (Err "Couldn't convert from DB to app types")
            )
        , fetchArchiveFailure (ArchiveFetched << Err)
        ]


getItemsAndScratch : Model -> RemoteData String ( List Item, Scratch )
getItemsAndScratch model =
    RemoteData.map3
        (\defs s archive ->
            case s of
                Nothing ->
                    let
                        def =
                            defaultScratch model.today
                    in
                    ( extractItems def.endDate archive defs, def )

                Just ( s_, _ ) ->
                    ( extractItems s_.endDate archive defs, s_ )
        )
        model.definitions
        model.scratch
        model.archive


itemsSection : Model -> Html Msg
itemsSection model =
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
                    [ Html.text "Loading" ]

                RemoteData.Failure _ ->
                    []
    in
    Html.node "main" [ Attr.class "flex flex-col space-y-4 w-full sm:w-auto" ] content


renderItem : Item -> Html Msg
renderItem item =
    Html.div [ Attr.class "card w-full sm:w-96 bg-base-100 shadow-xl" ]
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
scratchArea model =
    case getItemsAndScratch model of
        RemoteData.Loading ->
            Html.aside [] [ Html.text "Loading" ]

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
    Html.div [ Attr.class "w-screen sm:px-20 py-10" ]
        [ Html.header [ Attr.class "prose px-4 sm:px-auto lg:prose-xl flex space-x-4" ]
            [ Html.h2 [] [ Html.text "Home" ]
            ]
        , Html.div [ Attr.class "flex space-x-12 flex-col-reverse w-screen sm:flex-row" ]
            [ itemsSection model
            , Html.div [ Attr.class "p-4 sm:p-0" ] [ scratchArea model ]
            ]
        , Html.div [ Attr.class "toast toast-top toast-center" ]
            [ Toasty.view Toast.toastyConfig Toast.renderToast ToastyMsg model.toasties
            ]
        ]
