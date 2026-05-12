module Admin.DefinitionsPage exposing (..)

import BusinessLogic exposing (definitionDecoder, encodeDefinition, formatAmount, formatDate, frequencies, frequencyString, isError, parseFrequency, prettyPrintFrequency)
import Date exposing (Date, fromIsoString, toIsoString)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events exposing (onSubmit)
import Json.Decode as Decode
import Maybe.Extra as ME
import Numeral
import Ports.Definitions exposing (fetchDefinitions, fetchDefinitionsFailure, fetchDefinitionsSuccess, insertDefinition, saveDefinitionFailure, saveDefinitionSuccess, updateDefinition)
import Ports.Dialog exposing (closeDialog, openDialog)
import RemoteData exposing (RemoteData(..))
import Svg
import Svg.Attributes as SvgAttr
import Toast exposing (Toast)
import Toasty
import Types exposing (Definition, Frequency(..), SessionInfo)
import View.Common exposing (dialogCloseButton)


type alias EditingDefinition =
    { description : String
    , amount : String
    , frequency : Frequency
    , startDate : String
    , endDate : String
    , isAutomatic : Bool
    , isDeleted : Bool
    , definitionId : Maybe Int
    }


defaultEditingDefinition : Date -> EditingDefinition
defaultEditingDefinition today =
    { description = ""
    , amount = ""
    , frequency = OneTime
    , startDate = toIsoString today
    , endDate = ""
    , isAutomatic = False
    , isDeleted = False
    , definitionId = Nothing
    }


mkEditingDefinition : ( Definition, Int ) -> EditingDefinition
mkEditingDefinition ( def, id ) =
    { description = def.description
    , amount = Numeral.format "0.00" def.amount
    , frequency = def.frequency
    , startDate = toIsoString def.startDate
    , endDate =
        def.endDate
            |> Maybe.map toIsoString
            |> Maybe.withDefault ""
    , isAutomatic = def.isAutomatic
    , isDeleted = def.isDeleted
    , definitionId = Just id
    }


mkDefinition : EditingDefinition -> Maybe ( Definition, Maybe Int )
mkDefinition editingDefinition =
    Maybe.map3
        (\description amount startDate ->
            ( { description = description
              , amount = amount
              , frequency = editingDefinition.frequency
              , startDate = startDate
              , endDate = fromIsoString editingDefinition.endDate |> Result.toMaybe
              , isAutomatic = editingDefinition.isAutomatic
              , isDeleted = editingDefinition.isDeleted
              }
            , editingDefinition.definitionId
            )
        )
        (if editingDefinition.description == "" then
            Nothing

         else
            Just editingDefinition.description
        )
        (editingDefinition.amount |> String.toFloat)
        (fromIsoString editingDefinition.startDate |> Result.toMaybe)


type alias Model =
    { today : Date
    , toasties : Toasty.Stack (Toast Msg)
    , definitions : RemoteData String (List ( Definition, Int ))
    , editingDefinition : EditingDefinition
    }


type Msg
    = DefinitionsFetched (Result String (List ( Definition, Int )))
    | NewDefinition
    | EditDefinition ( Definition, Int )
    | SaveDefinition
    | SaveDefinitionSuccess ( Definition, Int )
    | SaveDefinitionFailed String
    | EditingDescriptionUpdated String
    | EditingIsAutomaticUpdated Bool
    | EditingIsDeletedUpdated Bool
    | EditingAmountUpdated String
    | EditingFrequencyChanged String
    | EditingStartDateChanged String
    | EditingEndDateChanged String
    | ToastyMsg (Toasty.Msg (Toast Msg))


init : Date -> ( Model, Cmd Msg )
init today =
    ( { today = today
      , toasties = Toasty.initialState
      , definitions = RemoteData.Loading
      , editingDefinition = defaultEditingDefinition today
      }
    , fetchDefinitions { includeDeleted = True }
    )


update : SessionInfo -> Msg -> Model -> ( Model, Cmd Msg )
update sessionInfo msg model =
    case msg of
        DefinitionsFetched results ->
            ( { model | definitions = RemoteData.fromResult results }, Cmd.none )
                |> Toasty.addToastIf Toast.toastyConfig ToastyMsg (\_ -> isError results) { severity = Toast.Error, message = Html.span [] [ Html.text "There was a problem fetching results. Please refresh the page." ] }

        NewDefinition ->
            ( { model | editingDefinition = defaultEditingDefinition model.today }, openDialog "definitionsModal" )

        EditDefinition ( def, id ) ->
            ( { model | editingDefinition = mkEditingDefinition ( def, id ) }, openDialog "definitionsModal" )

        SaveDefinition ->
            case mkDefinition model.editingDefinition of
                Just ( def, Just id ) ->
                    ( model, updateDefinition { data = encodeDefinition def, id = id } )

                Just ( def, Nothing ) ->
                    ( model, insertDefinition { data = encodeDefinition def, userId = sessionInfo.userId } )

                Nothing ->
                    ( model, Cmd.none )

        SaveDefinitionSuccess ( def, id ) ->
            let
                updatedDefinitions defs =
                    if List.any (\( _, id_ ) -> id == id_) defs then
                        defs
                            |> List.map
                                (\( d, id_ ) ->
                                    if id == id_ then
                                        ( def, id_ )

                                    else
                                        ( d, id_ )
                                )

                    else
                        ( def, id ) :: defs
            in
            ( { model
                | definitions = RemoteData.map updatedDefinitions model.definitions
                , editingDefinition = defaultEditingDefinition model.today
              }
            , closeDialog "definitionsModal"
            )

        SaveDefinitionFailed _ ->
            ( model, Cmd.none )
                |> Toasty.addToast Toast.toastyConfig ToastyMsg { severity = Toast.Error, message = Html.span [] [ Html.text "There was a problem saving the definition. Please try again." ] }

        EditingDescriptionUpdated str ->
            let
                editingDefinition =
                    model.editingDefinition
            in
            ( { model | editingDefinition = { editingDefinition | description = str } }, Cmd.none )

        EditingIsAutomaticUpdated p ->
            let
                editingDefinition =
                    model.editingDefinition
            in
            ( { model | editingDefinition = { editingDefinition | isAutomatic = p } }, Cmd.none )

        EditingIsDeletedUpdated p ->
            let
                editingDefinition =
                    model.editingDefinition
            in
            ( { model | editingDefinition = { editingDefinition | isDeleted = p } }, Cmd.none )

        EditingAmountUpdated str ->
            let
                editingDefinition =
                    model.editingDefinition
            in
            ( { model | editingDefinition = { editingDefinition | amount = str } }, Cmd.none )

        EditingFrequencyChanged str ->
            let
                editingDefinition =
                    model.editingDefinition
            in
            ( { model
                | editingDefinition =
                    { editingDefinition
                        | frequency =
                            parseFrequency str
                                |> Result.withDefault editingDefinition.frequency
                    }
              }
            , Cmd.none
            )

        EditingStartDateChanged str ->
            let
                editingDefinition =
                    model.editingDefinition
            in
            ( { model | editingDefinition = { editingDefinition | startDate = str } }
            , Cmd.none
            )

        EditingEndDateChanged str ->
            let
                editingDefinition =
                    model.editingDefinition
            in
            ( { model | editingDefinition = { editingDefinition | endDate = str } }, Cmd.none )

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
        , saveDefinitionSuccess
            (\raw ->
                case
                    Decode.decodeValue definitionDecoder raw
                of
                    Ok def ->
                        SaveDefinitionSuccess def

                    Err _ ->
                        SaveDefinitionFailed "Couldn't convert from DB to app types"
            )
        , saveDefinitionFailure SaveDefinitionFailed
        ]


renderDefinitionRow : ( Definition, Int ) -> Html Msg
renderDefinitionRow ( def, id ) =
    Html.tr [ Attr.class "hover cursor-pointer", Events.onClick (EditDefinition ( def, id )) ]
        [ Html.th [ Attr.class "flex justify-between" ]
            [ Html.span [] [ Html.text def.description ]
            , if def.isAutomatic then
                Html.div [ Attr.class "badge badge-primary" ] [ Html.text "Automatic" ]

              else
                Html.text ""
            ]
        , Html.td [] [ Html.text (formatAmount def.amount) ]
        , Html.td [] [ Html.text (prettyPrintFrequency def.frequency) ]
        , Html.td [] [ Html.text (formatDate def.startDate) ]
        , Html.td []
            [ def.endDate
                |> Maybe.map formatDate
                |> Maybe.withDefault "--"
                |> Html.text
            ]
        , Html.td []
            [ if def.isDeleted then
                Html.text "Y"

              else
                Html.text "N"
            ]
        ]


definitionsModal : ( EditingDefinition, Maybe Int ) -> Html Msg
definitionsModal ( def, mId ) =
    Html.node "dialog"
        [ Attr.class "modal", Attr.id "definitionsModal" ]
        [ Html.div [ Attr.class "modal-box" ]
            [ dialogCloseButton
            , Html.h3 [ Attr.class "font-bold text-lg" ]
                [ if ME.isJust mId then
                    Html.text "Edit Definition"

                  else
                    Html.text "New Definition"
                ]
            , Html.form
                [ Attr.class "flex flex-col space-y-2"
                , onSubmit SaveDefinition
                ]
                [ Html.label [ Attr.class "form-control w-full max-w-xs" ]
                    [ Html.input
                        [ Attr.class "input input-bordered w-full max-w-xs"
                        , Attr.placeholder "Description"
                        , Attr.required True
                        , Attr.value def.description
                        , Events.onInput EditingDescriptionUpdated
                        ]
                        []
                    ]
                , Html.div [ Attr.class "form-control w-full max-w-xs" ]
                    [ Html.label [ Attr.class "label cursor-pointer" ]
                        [ Html.span [ Attr.class "label-text" ]
                            [ Html.text "Is Automatic?" ]
                        , Html.input
                            [ Attr.class "checkbox"
                            , Attr.type_ "checkbox"
                            , Attr.checked def.isAutomatic
                            , Events.onCheck EditingIsAutomaticUpdated
                            ]
                            []
                        ]
                    ]
                , Html.input
                    [ Attr.class "input input-bordered w-full max-w-xs"
                    , Attr.placeholder "Amount"
                    , Attr.required True
                    , Attr.value def.amount
                    , Events.onInput EditingAmountUpdated
                    ]
                    []
                , frequencies
                    |> List.map (renderFrequencyOption def.frequency)
                    |> Html.select
                        [ Attr.class "select select-bordered w-full max-w-xs"
                        , Events.onInput EditingFrequencyChanged
                        ]
                , Html.input
                    [ Attr.class "input input-bordered w-full max-w-xs"
                    , Attr.type_ "date"
                    , Attr.required True
                    , Attr.value def.startDate
                    , Events.onInput EditingStartDateChanged
                    ]
                    []
                , Html.input
                    [ Attr.class "input input-bordered w-full max-w-xs"
                    , Attr.type_ "date"
                    , Attr.value def.endDate
                    , Events.onInput EditingEndDateChanged
                    ]
                    []
                , Html.div [ Attr.class "form-control" ]
                    [ Html.button
                        [ Attr.class "btn"
                        , Attr.classList [ ( "btn-error", not def.isDeleted ), ( "btn-warning", def.isDeleted ) ]
                        , Attr.type_ "button"
                        , Events.onClick (EditingIsDeletedUpdated (not def.isDeleted))
                        ]
                        [ Html.text
                            (if def.isDeleted then
                                "Undelete"

                             else
                                "Delete"
                            )
                        ]
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
        , Html.form [ Attr.method "dialog", Attr.class "modal-backdrop" ]
            [ Html.button [] [ Html.text "close" ]
            ]
        ]


renderFrequencyOption : Frequency -> Frequency -> Html Msg
renderFrequencyOption selectedFrequency freq =
    Html.option
        [ Attr.selected (freq == selectedFrequency)
        , Attr.value (frequencyString freq)
        ]
        [ Html.text (prettyPrintFrequency freq) ]


view : Model -> Html Msg
view model =
    Html.div [ Attr.class "w-screen sm:px-20 py-10" ]
        [ Html.header [ Attr.class "prose px-4 sm:px-auto lg:prose-xl flex space-x-4" ]
            [ Html.h2 []
                [ Html.text "Definitions"
                ]
            , Html.button [ Attr.class "btn btn-primary", Events.onClick NewDefinition ]
                [ Html.text "New"
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
                        , SvgAttr.d "M5 12h14m-7 7V5"
                        ]
                        []
                    ]
                ]
            ]
        , Html.node "main"
            []
            (case model.definitions of
                RemoteData.Loading ->
                    [ Html.text "Loading" ]

                RemoteData.Success s ->
                    if List.isEmpty s then
                        [ Html.p [] [ Html.text "No definitions found" ] ]

                    else
                        [ Html.div [ Attr.class "hidden lg:block" ] [ tableView s ]
                        , Html.div [ Attr.class "lg:hidden" ] [ cardView s ]
                        ]

                _ ->
                    [ Html.text "" ]
            )
        , definitionsModal ( model.editingDefinition, Nothing )
        , Html.div [ Attr.class "toast toast-top toast-center" ]
            [ Toasty.view Toast.toastyConfig Toast.renderToast ToastyMsg model.toasties
            ]
        ]


tableView : List ( Definition, Int ) -> Html Msg
tableView s =
    Html.table [ Attr.class "table table-zebra" ]
        [ Html.thead []
            [ Html.tr []
                [ Html.th [] [ Html.text "Description" ]
                , Html.th [] [ Html.text "Amount" ]
                , Html.th [] [ Html.text "Frequency" ]
                , Html.th [] [ Html.text "Start Date" ]
                , Html.th [] [ Html.text "End Date" ]
                , Html.th [] [ Html.text "Deleted?" ]
                ]
            ]
        , s
            |> List.map renderDefinitionRow
            |> Html.tbody []
        ]


renderDefinitionCard : ( Definition, Int ) -> Html Msg
renderDefinitionCard ( def, id ) =
    Html.div [ Attr.class "card w-full sm:w-96 bg-base-100 shadow-xl cursor-pointer", Events.onClick (EditDefinition ( def, id )) ]
        [ Html.div [ Attr.class "card-body" ]
            [ Html.h2 [ Attr.class "card-title flex justify-between" ]
                [ Html.span [] [ Html.text def.description ]
                , Html.span [] [ def.amount |> formatAmount |> Html.text ]
                ]
            , Html.p [] [ def.startDate |> formatDate |> Html.text ]
            , Html.div [ Attr.class "flex justify-between items-center" ]
                [ Html.div []
                    [ if def.isAutomatic then
                        Html.div [ Attr.class "badge badge-primary" ] [ Html.text "Automatic" ]

                      else
                        Html.text ""
                    ]
                ]
            ]
        ]


cardView : List ( Definition, Int ) -> Html Msg
cardView s =
    s
        |> List.map renderDefinitionCard
        |> Html.div [ Attr.class "w-full flex flex-col space-y-4 sm:w-auto" ]
