module BusinessLogic exposing (..)

import Date exposing (Date, Unit(..), add, fromIsoString)
import List.Extra as LE
import Time exposing (Month(..))
import Types exposing (BudgetDefinition, Frequency(..), Item, RawDefinition, RawScratch, Scratch)


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
    , amountAvailable = 0
    , amountLeftOver = 0
    }


computeResults : List Item -> Scratch -> { totalOwing : Float, totalOutstanding : Float }
computeResults items scratch =
    let
        totalOwing =
            items |> List.foldr (\item total -> item.definition.amount + total) 0
    in
    { totalOwing = totalOwing, totalOutstanding = min 0 (scratch.amountAvailable - scratch.amountLeftOver - totalOwing) }


convertRawScratch : RawScratch -> Maybe Scratch
convertRawScratch raw =
    Maybe.map (\endDate -> Scratch endDate raw.amountAvailable raw.amountLeftOver)
        (fromIsoString raw.endDate |> Result.toMaybe)


convertRawDefinition : RawDefinition -> Maybe BudgetDefinition
convertRawDefinition raw =
    Maybe.map3
        (\startDate endDate frequency ->
            { startDate = startDate
            , endDate = endDate
            , description = raw.description
            , amount = raw.amount
            , frequency = frequency
            , isAutomatic = raw.isAutomatic
            }
        )
        (fromIsoString raw.startDate |> Result.toMaybe)
        (raw.endDate |> Maybe.map (fromIsoString >> Result.toMaybe))
        (parseFrequency raw.frequency)


parseFrequency : String -> Maybe Frequency
parseFrequency str =
    case str of
        "onetime" ->
            Just OneTime

        "weekly" ->
            Just Weekly

        "biweekly" ->
            Just BiWeekly

        "monthly" ->
            Just Monthly

        _ ->
            Nothing
