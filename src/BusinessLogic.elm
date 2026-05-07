module BusinessLogic exposing (..)

import Date exposing (Date, Unit(..), add, fromIsoString, toIsoString)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as LE
import Time exposing (Month(..))
import Types exposing (ArchiveAction(..), BudgetDefinition, Frequency(..), Item, Scratch, SessionInfo)


extractItems : Date -> List ( BudgetDefinition, Int ) -> List Item
extractItems endDate defs =
    defs
        |> LE.andThen (\( def, id ) -> def |> extractDatesForDefinition endDate |> List.map (extractItem def id))
        |> List.sortBy (\item -> Date.toRataDie item.date)


extractItem : BudgetDefinition -> Int -> Date -> Item
extractItem def defId date =
    { date = date, definition = def, definitionId = defId }


extractDatesForDefinition : Date -> BudgetDefinition -> List Date
extractDatesForDefinition endDate def =
    let
        go date =
            if Date.toRataDie date <= Date.toRataDie endDate then
                case def.frequency of
                    OneTime ->
                        [ date ]

                    Weekly ->
                        date :: go (Date.add Days 7 date)

                    BiWeekly ->
                        date :: go (Date.add Days 14 date)

                    Monthly ->
                        date :: go (Date.add Months 1 date)

            else
                []
    in
    go def.startDate


defaultScratch : Date -> Scratch
defaultScratch today =
    { endDate = add Days 14 today
    , amountInBank = 0
    , amountLeftOver = 0
    }


computeResults : List Item -> Scratch -> { totalOwing : Float, totalOutstanding : Float }
computeResults items scratch =
    let
        totalOwing =
            items |> List.foldr (\item total -> item.definition.amount + total) 0
    in
    { totalOwing = totalOwing, totalOutstanding = abs (min 0 (scratch.amountInBank - scratch.amountLeftOver - totalOwing)) }


dateDecoder : Decode.Decoder Date
dateDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case fromIsoString str of
                    Ok d ->
                        Decode.succeed d

                    Err err ->
                        Decode.fail err
            )


rawScratchDecoder : Decode.Decoder ( Scratch, Int )
rawScratchDecoder =
    Decode.map4 (\endDate amountInBank amountLeftOver id -> ( Scratch endDate amountInBank amountLeftOver, id ))
        (Decode.field "endDate" dateDecoder)
        (Decode.field "amountInBank" Decode.float)
        (Decode.field "amountLeftOver" Decode.float)
        (Decode.field "id" Decode.int)


encodeDate : Date -> Encode.Value
encodeDate date =
    Encode.string (toIsoString date)


encodeScratch : Scratch -> Encode.Value
encodeScratch scratch =
    Encode.object
        [ ( "endDate", encodeDate scratch.endDate )
        , ( "amountInBank", Encode.float scratch.amountInBank )
        , ( "amountLeftOver", Encode.float scratch.amountLeftOver )
        ]


rawDefinitionDecoder : Decode.Decoder ( BudgetDefinition, Int )
rawDefinitionDecoder =
    Decode.map7 (\sd ed d a f auto id -> ( BudgetDefinition sd ed d a f auto, id ))
        (Decode.field "startDate" dateDecoder)
        (Decode.field "endDate" (Decode.nullable dateDecoder))
        (Decode.field "description" Decode.string)
        (Decode.field "amount" Decode.float)
        (Decode.field "frequency" frequencyDecoder)
        (Decode.field "isAutomatic" Decode.bool)
        (Decode.field "id" Decode.int)


sessionInfoDecoder : Decode.Decoder SessionInfo
sessionInfoDecoder =
    Decode.map3 SessionInfo
        (Decode.field "email" Decode.string)
        (Decode.field "userId" Decode.string)
        (Decode.field "confirmed" Decode.bool)


frequencyDecoder : Decode.Decoder Frequency
frequencyDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "onetime" ->
                        Decode.succeed OneTime

                    "weekly" ->
                        Decode.succeed Weekly

                    "biweekly" ->
                        Decode.succeed BiWeekly

                    "monthly" ->
                        Decode.succeed Monthly

                    _ ->
                        Decode.fail "Couldn't decode frequency"
            )


encodeAction : ArchiveAction -> Encode.Value
encodeAction action =
    case action of
        PayAction ->
            Encode.string "pay"

        SkipAction ->
            Encode.string "skip"


encodeArchive : Item -> ArchiveAction -> Encode.Value
encodeArchive item action =
    Encode.object
        [ ( "definitionId", Encode.int item.definitionId )
        , ( "description", Encode.string item.definition.description )
        , ( "amount", Encode.float item.definition.amount )
        , ( "date", encodeDate item.date )
        , ( "action", encodeAction action )
        ]
