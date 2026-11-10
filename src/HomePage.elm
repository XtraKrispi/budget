module HomePage exposing (..)

import BusinessLogic exposing (amountRemaining, archiveDecoder, computeResults, defaultScratch, definitionDecoder, encodeArchive, encodeScratch, extractItems, formatAmount, formatDate, isError, scratchDecoder)
import Date exposing (Date, fromIsoString, toIsoString, toRataDie)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events exposing (onSubmit)
import Json.Decode as Decode
import Numeral
import Ports.Archive exposing (fetchArchive, fetchArchiveFailure, fetchArchiveSuccess, insertArchive, insertArchiveFailure, insertArchiveSuccess)
import Ports.Clipboard exposing (copyToClipboard)
import Ports.Definitions exposing (fetchDefinitions, fetchDefinitionsFailure, fetchDefinitionsSuccess)
import Ports.Dialog exposing (closeDialog, openDialog)
import Ports.Scratch exposing (fetchScratch, fetchScratchFailure, fetchScratchSuccess, insertScratch, saveScratchFailure, saveScratchSuccess, updateScratch)
import RemoteData exposing (RemoteData)
import Svg
import Svg.Attributes as SvgAttr
import Toast exposing (Toast)
import Toasty
import Types exposing (Archive, ArchiveAction(..), Definition, Item, Scratch, SessionInfo)
import View.Common exposing (dialogCloseButton)


type alias Model =
    { definitions : RemoteData String (List ( Definition, Int ))
    , scratch : RemoteData String (Maybe ( Scratch, Int ))
    , archive : RemoteData String (List Archive)
    , today : Date
    , editingScratch : EditingScratch
    , toasties : Toasty.Stack (Toast Msg)
    , editingPartialPayment : Maybe Partial
    }


type alias Partial =
    { item : Item
    , amount : String
    }


type alias EditingScratch =
    { endDate : String
    , amountInBank : String
    , amountLeftOver : String
    , comments : String
    }


type Msg
    = CopyAmount Float
    | Skip Item
    | Pay Item
    | OpenPartialPayments Item
    | PartialPayment
    | PartialPaymentUpdated String
    | RecalculateTotals
    | RecalculateSuccess Int
    | RecalculateFailed String
    | DefinitionsFetched (Result String (List ( Definition, Int )))
    | ScratchFetched (Result String (Maybe ( Scratch, Int )))
    | ArchiveFetched (Result String (List Archive))
    | ScratchDateUpdated String
    | ScratchAmountInBankUpdated String
    | ScratchAmountLeftOverUpdated String
    | ScratchCommentsUpdated String
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
      , editingPartialPayment = Nothing
      }
    , Cmd.batch
        [ fetchDefinitions { includeDeleted = False }
        , fetchScratch ()
        , fetchArchive { includeDeletedDefinitions = False }
        ]
    )


toEditingScratch : Scratch -> EditingScratch
toEditingScratch { endDate, amountInBank, amountLeftOver, comments } =
    { endDate = toIsoString endDate
    , amountInBank = Numeral.format "0.00" amountInBank
    , amountLeftOver = Numeral.format "0.00" amountLeftOver
    , comments = comments
    }


toScratch : EditingScratch -> Maybe Scratch
toScratch { endDate, amountInBank, amountLeftOver, comments } =
    Maybe.map4 Scratch
        (fromIsoString endDate |> Result.toMaybe)
        (String.toFloat amountInBank)
        (String.toFloat amountLeftOver)
        (Just comments)


update : SessionInfo -> Msg -> Model -> ( Model, Cmd Msg )
update sessionInfo msg model =
    case msg of
        CopyAmount amt ->
            ( model, copyToClipboard (String.fromFloat amt) )

        Skip item ->
            ( model, insertArchive { data = encodeArchive item SkipAction Nothing, userId = sessionInfo.userId } )

        Pay item ->
            ( model, insertArchive { data = encodeArchive item PayAction Nothing, userId = sessionInfo.userId } )

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

        ScratchCommentsUpdated str ->
            let
                editingScratch =
                    model.editingScratch
            in
            ( { model
                | editingScratch =
                    { editingScratch
                        | comments = str
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

        PartialPayment ->
            case ( model.editingPartialPayment, Maybe.andThen (String.toFloat << .amount) model.editingPartialPayment ) of
                ( Just { item }, Just amount ) ->
                    ( model
                    , Cmd.batch
                        [ insertArchive { data = encodeArchive item PayAction (Just amount), userId = sessionInfo.userId }
                        , closeDialog "partialPaymentsModal"
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        OpenPartialPayments item ->
            ( { model
                | editingPartialPayment =
                    Just
                        { item = item
                        , amount = ""
                        }
              }
            , openDialog "partialPaymentsModal"
            )

        PartialPaymentUpdated str ->
            ( { model | editingPartialPayment = Maybe.map (\p -> { p | amount = str }) model.editingPartialPayment }, Cmd.none )


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
    Html.div [ Attr.class "card w-full sm:w-150 bg-base-100 shadow-xl" ]
        [ Html.div [ Attr.class "card-body" ]
            [ Html.h2 [ Attr.class "card-title flex justify-between" ]
                [ Html.span [] [ Html.text item.definition.description ]
                , Html.div [ Attr.class "flex gap-2" ]
                    [ Html.span [ Attr.class "cursor-pointer", Events.onClick (CopyAmount (amountRemaining item)) ]
                        [ item
                            |> amountRemaining
                            |> formatAmount
                            |> Html.text
                        ]
                    , if List.isEmpty item.payments then
                        Html.text ""

                      else
                        Svg.svg
                            [ SvgAttr.fill "none"
                            , SvgAttr.viewBox "0 0 24 24"
                            , SvgAttr.strokeWidth "1.5"
                            , SvgAttr.stroke "currentColor"
                            , SvgAttr.class "size-6 text-info"
                            , SvgAttr.title "Partial payments exist for this item"
                            ]
                            [ Svg.path
                                [ SvgAttr.strokeLinecap "round"
                                , SvgAttr.strokeLinejoin "round"
                                , SvgAttr.d "m11.25 11.25.041-.02a.75.75 0 0 1 1.063.852l-.708 2.836a.75.75 0 0 0 1.063.853l.041-.021M21 12a9 9 0 1 1-18 0 9 9 0 0 1 18 0Zm-9-3.75h.008v.008H12V8.25Z"
                                ]
                                []
                            ]
                    ]
                ]
            , Html.p [] [ item.date |> formatDate |> Html.text ]
            , Html.div [ Attr.class "flex justify-between items-center" ]
                [ Html.div []
                    [ if item.definition.isAutomatic then
                        Html.div [ Attr.class "badge badge-primary" ] [ Html.text "Automatic" ]

                      else
                        Html.text ""
                    ]
                , Html.div [ Attr.class "flex flex-col sm:flex-row gap-2 justify-end" ]
                    [ Html.button
                        [ Attr.class "btn"
                        , Events.onClick (Skip item)
                        ]
                        [ Html.text "Skip" ]
                    , Html.button
                        [ Attr.class "btn btn-primary"
                        , Events.onClick (Pay item)
                        ]
                        [ Html.text "Pay Remainder" ]
                    , Html.button
                        [ Attr.class "btn btn-info"
                        , Events.onClick (OpenPartialPayments item)
                        ]
                        [ Html.text "Partial Payments" ]
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
            Html.aside [ Attr.class "prose flex flex-col space-y-4 lg:min-w-125 lg:w-auto w-full " ]
                [ Html.form
                    [ Attr.class "flex flex-col gap-4"
                    , Events.onSubmit RecalculateTotals
                    ]
                    [ Html.div [ Attr.class "flex lg:flex-row flex-col gap-4" ]
                        [ Html.div [ Attr.class "flex flex-col space-y-4" ]
                            [ Html.input
                                [ Attr.class "input input-bordered w-full sm:max-w-xs"
                                , Attr.type_ "date"
                                , Attr.value model.editingScratch.endDate
                                , Events.onInput ScratchDateUpdated
                                ]
                                []
                            , Html.input
                                [ Attr.class "input input-bordered w-full sm:max-w-xs"
                                , Attr.placeholder "Amount in account"
                                , Attr.value model.editingScratch.amountInBank
                                , Events.onInput ScratchAmountInBankUpdated
                                ]
                                []
                            , Html.input
                                [ Attr.class "input input-bordered w-full sm:max-w-xs"
                                , Attr.placeholder "Amount to be left over"
                                , Attr.value model.editingScratch.amountLeftOver
                                , Events.onInput ScratchAmountLeftOverUpdated
                                ]
                                []
                            ]
                        , Html.textarea
                            [ Attr.class "textarea w-full"
                            , Attr.value model.editingScratch.comments
                            , Attr.placeholder "Any relevant comments here..."
                            , Events.onInput ScratchCommentsUpdated
                            ]
                            []
                        ]
                    , Html.button
                        [ Attr.class "btn btn-primary"
                        , Attr.type_ "submit"
                        ]
                        [ Html.text "Save + Recalculate" ]
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


partialPaymentModal : Maybe Partial -> Html Msg
partialPaymentModal part =
    Html.node "dialog"
        [ Attr.class "modal", Attr.id "partialPaymentsModal" ]
        [ Html.div [ Attr.class "modal-box" ]
            [ dialogCloseButton
            , Html.h3 [ Attr.class "font-bold text-lg" ]
                [ Html.text "Partial Payments"
                ]
            , Html.div [ Attr.class "flex flex-col gap-2" ]
                [ case part of
                    Just p ->
                        let
                            totalPaid =
                                p.item.payments |> List.map Tuple.first |> List.sum
                        in
                        Html.div [ Attr.class "overflow-x-auto rounded-box border border-base-content/5 bg-base-100" ]
                            [ Html.table [ Attr.class "table" ]
                                [ Html.thead []
                                    [ Html.tr []
                                        [ Html.th [] [ Html.text "Date" ]
                                        , Html.th [] [ Html.text "Amount" ]
                                        ]
                                    ]
                                , p.item.payments
                                    |> List.sortBy (toRataDie << Tuple.second)
                                    |> List.map
                                        (\( amount, date ) ->
                                            Html.tr []
                                                [ Html.td [] [ date |> formatDate |> Html.text ]
                                                , Html.td [ Attr.class "text-right" ]
                                                    [ amount
                                                        |> formatAmount
                                                        |> Html.text
                                                    ]
                                                ]
                                        )
                                    |> Html.tbody []
                                , Html.tfoot []
                                    [ Html.tr [] [ Html.th [] [ Html.text "Total Partial Payments" ], Html.th [ Attr.class "text-right" ] [ totalPaid |> formatAmount |> Html.text ] ]
                                    , Html.tr [] [ Html.th [] [ Html.text "Amount Remaining" ], Html.th [ Attr.class "text-right" ] [ p.item.definition.amount - totalPaid |> formatAmount |> Html.text ] ]
                                    ]
                                ]
                            ]

                    Nothing ->
                        Html.text ""
                , Html.form
                    [ Attr.class "flex flex-col space-y-2"
                    , onSubmit PartialPayment
                    ]
                    [ Html.label [ Attr.class "form-control w-full max-w-xs" ]
                        [ Html.input
                            [ Attr.class "input input-bordered w-full max-w-xs"
                            , Attr.placeholder "Amount"
                            , Attr.required True
                            , Attr.value (part |> Maybe.map .amount |> Maybe.withDefault "")
                            , Events.onInput PartialPaymentUpdated
                            ]
                            []
                        ]
                    , Html.div [ Attr.class "modal-action" ]
                        [ Html.button
                            [ Attr.class "btn btn-primary"
                            , Attr.type_ "submit"
                            ]
                            [ Html.text "Save"
                            , Svg.svg
                                [ SvgAttr.class "h-6 w-6"
                                , SvgAttr.fill "none"
                                , SvgAttr.viewBox "0 0 24 24"
                                , SvgAttr.strokeWidth "1.5"
                                , SvgAttr.stroke "currentColor"
                                ]
                                [ Svg.path
                                    [ SvgAttr.strokeLinecap "round"
                                    , SvgAttr.strokeLinejoin "round"
                                    , SvgAttr.strokeWidth "2"
                                    , SvgAttr.d "M11 16h2m6.707-9.293-2.414-2.414A1 1 0 0 0 16.586 4H5a1 1 0 0 0-1 1v14a1 1 0 0 0 1 1h14a1 1 0 0 0 1-1V7.414a1 1 0 0 0-.293-.707ZM16 20v-6a1 1 0 0 0-1-1H9a1 1 0 0 0-1 1v6h8ZM9 4h6v3a1 1 0 0 1-1 1h-4a1 1 0 0 1-1-1V4Z"
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , Html.form [ Attr.method "dialog", Attr.class "modal-backdrop" ]
            [ Html.button [] [ Html.text "close" ]
            ]
        ]


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
        , partialPaymentModal model.editingPartialPayment
        ]
