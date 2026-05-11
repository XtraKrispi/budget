module BusinessLogic exposing (..)

import Date exposing (Date, Unit(..), add, fromIsoString, toIsoString)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as LE
import Numeral
import Time exposing (Month(..))
import Types exposing (Archive, ArchiveAction(..), Definition, Frequency(..), Item, Scratch, SessionInfo)


extractItems : Date -> List Archive -> List ( Definition, Int ) -> List Item
extractItems endDate archive defs =
    defs
        |> LE.andThen (\( def, id ) -> def |> extractDatesForDefinition endDate |> List.map (extractItem def id))
        |> List.filter
            (\item ->
                not
                    (List.any (\a -> a.date == item.date && a.definitionId == item.definitionId)
                        archive
                    )
            )
        |> List.sortBy (\item -> Date.toRataDie item.date)


extractItem : Definition -> Int -> Date -> Item
extractItem def defId date =
    { date = date, definition = def, definitionId = defId }


extractDatesForDefinition : Date -> Definition -> List Date
extractDatesForDefinition endDate def =
    let
        go date =
            if
                (Maybe.map (\ed -> Date.toRataDie date <= Date.toRataDie ed) def.endDate
                    |> Maybe.withDefault True
                )
                    && Date.toRataDie date
                    <= Date.toRataDie endDate
            then
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


scratchDecoder : Decode.Decoder ( Scratch, Int )
scratchDecoder =
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


definitionDecoder : Decode.Decoder ( Definition, Int )
definitionDecoder =
    Decode.map7 (\sd ed d a f auto id -> ( Definition sd ed d a f auto, id ))
        (Decode.field "startDate" dateDecoder)
        (Decode.field "endDate" (Decode.nullable dateDecoder))
        (Decode.field "description" Decode.string)
        (Decode.field "amount" Decode.float)
        (Decode.field "frequency" frequencyDecoder)
        (Decode.field "isAutomatic" Decode.bool)
        (Decode.field "id" Decode.int)


encodeDefinition : Definition -> Encode.Value
encodeDefinition def =
    Encode.object
        [ ( "startDate", Encode.string (toIsoString def.startDate) )
        , ( "endDate"
          , def.endDate
                |> Maybe.map (Encode.string << toIsoString)
                |> Maybe.withDefault Encode.null
          )
        , ( "description", Encode.string def.description )
        , ( "amount", Encode.float def.amount )
        , ( "frequency", Encode.string (frequencyString def.frequency) )
        , ( "isAutomatic", Encode.bool def.isAutomatic )
        ]


sessionInfoDecoder : Decode.Decoder SessionInfo
sessionInfoDecoder =
    Decode.map3 SessionInfo
        (Decode.field "email" Decode.string)
        (Decode.field "userId" Decode.string)
        (Decode.field "confirmed" Decode.bool)


parseFrequency : String -> Result String Frequency
parseFrequency str =
    case str of
        "onetime" ->
            Ok OneTime

        "weekly" ->
            Ok Weekly

        "biweekly" ->
            Ok BiWeekly

        "monthly" ->
            Ok Monthly

        _ ->
            Err "Couldn't decode frequency"


frequencyDecoder : Decode.Decoder Frequency
frequencyDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case parseFrequency str of
                    Ok f ->
                        Decode.succeed f

                    Err s ->
                        Decode.fail s
            )


encodeAction : ArchiveAction -> Encode.Value
encodeAction action =
    case action of
        PayAction ->
            Encode.string "pay"

        SkipAction ->
            Encode.string "skip"


actionDecoder : Decode.Decoder ArchiveAction
actionDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "pay" ->
                        Decode.succeed PayAction

                    "skip" ->
                        Decode.succeed SkipAction

                    _ ->
                        Decode.fail "invalid archive action"
            )


encodeArchive : Item -> ArchiveAction -> Encode.Value
encodeArchive item action =
    Encode.object
        [ ( "definitionId", Encode.int item.definitionId )
        , ( "description", Encode.string item.definition.description )
        , ( "amount", Encode.float item.definition.amount )
        , ( "date", encodeDate item.date )
        , ( "action", encodeAction action )
        ]


archiveDecoder : Decode.Decoder Archive
archiveDecoder =
    Decode.map5 Archive
        (Decode.field "date" dateDecoder)
        (Decode.field "definitionId" Decode.int)
        (Decode.field "description" Decode.string)
        (Decode.field "amount" Decode.float)
        (Decode.field "action" actionDecoder)


isError : Result e a -> Bool
isError r =
    case r of
        Ok _ ->
            False

        _ ->
            True


formatAmount : Float -> String
formatAmount =
    Numeral.format "$0,0.00"


formatDate : Date -> String
formatDate =
    Date.format "Y-MM-dd"


prettyPrintFrequency : Frequency -> String
prettyPrintFrequency freq =
    case freq of
        OneTime ->
            "One Time"

        Weekly ->
            "Weekly"

        BiWeekly ->
            "Bi-Weekly"

        Monthly ->
            "Monthly"


frequencyString : Frequency -> String
frequencyString freq =
    case freq of
        OneTime ->
            "onetime"

        Weekly ->
            "weekly"

        BiWeekly ->
            "biweekly"

        Monthly ->
            "monthly"


frequencies : List Frequency
frequencies =
    [ OneTime, Weekly, BiWeekly, Monthly ]
