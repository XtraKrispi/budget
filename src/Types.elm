module Types exposing (..)

import Date exposing (Date)


type alias SessionInfo =
    { email : String
    , userId : String
    , confirmed : Bool
    }


type Frequency
    = OneTime
    | Weekly
    | BiWeekly
    | Monthly


type alias BudgetDefinition =
    { startDate : Date
    , endDate : Maybe Date
    , description : String
    , amount : Float
    , frequency : Frequency
    , isAutomatic : Bool
    }


type alias Item =
    { date : Date
    , definition : BudgetDefinition
    , definitionId : Int
    }


type alias Scratch =
    { endDate : Date
    , amountInBank : Float
    , amountLeftOver : Float
    }


type alias RawDefinition =
    { startDate : String
    , endDate : Maybe String
    , description : String
    , amount : Float
    , frequency : String
    , isAutomatic : Bool
    }


type alias RawScratch =
    { endDate : String
    , amountInBank : Float
    , amountLeftOver : Float
    }


type ArchiveAction
    = PayAction
    | SkipAction
