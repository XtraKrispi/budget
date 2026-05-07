module BusinessLogic exposing (..)

import Date exposing (Date, Unit(..), add, fromIsoString, toIsoString)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as LE
import Time exposing (Month(..))
import Types exposing (BudgetDefinition, Frequency(..), Item, Scratch, SessionInfo)


extractItems : Date -> List BudgetDefinition -> List Item
extractItems endDate defs =
    defs
        |> LE.andThen (\def -> def |> extractDatesForDefinition endDate |> List.map (extractItem def))
        |> List.sortBy (\item -> Date.toRataDie item.date)


extractItem : BudgetDefinition -> Date -> Item
extractItem def date =
    { date = date, definition = def }


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


encodeScratch : Scratch -> Encode.Value
encodeScratch scratch =
    Encode.object
        [ ( "endDate", Encode.string (toIsoString scratch.endDate) )
        , ( "amountInBank", Encode.float scratch.amountInBank )
        , ( "amountLeftOver", Encode.float scratch.amountLeftOver )
        ]


rawDefinitionDecoder : Decode.Decoder BudgetDefinition
rawDefinitionDecoder =
    Decode.map6 BudgetDefinition
        (Decode.field "startDate" dateDecoder)
        (Decode.field "endDate" (Decode.nullable dateDecoder))
        (Decode.field "description" Decode.string)
        (Decode.field "amount" Decode.float)
        (Decode.field "frequency" frequencyDecoder)
        (Decode.field "isAutomatic" Decode.bool)


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
