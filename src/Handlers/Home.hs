module Handlers.Home (getHome, postHome) where

import AppError (AppError)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Time (Day, addDays, addGregorianMonthsClip)
import Effectful
import Effectful.Error.Static (Error)
import Effects.ArchiveStore
import Effects.DefinitionStore
import Effects.ScratchStore
import Effects.Time
import Handlers.Model
import Handlers.Utils (getParam, htmlResponse)
import Html.Home qualified as Home
import Htmx.Request (isBoosted, isHtmx)
import Model

getHome ::
  ( DefinitionStore :> es
  , Time :> es
  , ArchiveStore :> es
  , ScratchStore :> es
  ) =>
  Request ->
  User ->
  Eff es Response
getHome request user = do
  let htmx = isHtmx request
  let boosted = isBoosted request
  if not boosted && htmx
    then do
      homeContent user
    else pure $ htmlResponse $ Home.homePage user

postHome ::
  ( DefinitionStore :> es
  , ArchiveStore :> es
  , ScratchStore :> es
  , Time :> es
  , Error AppError :> es
  ) =>
  Request ->
  User ->
  Eff es Response
postHome request user = do
  endDate <- unMyDay <$> getParam request "end-date"
  amountInBank <- getParam request "amount-in-bank"
  amountLeftOver <- getParam request "amount-left-over"

  let scratch = Scratch endDate amountInBank amountLeftOver
  Effects.ScratchStore.save user.email scratch
  homeContent user

homeContent ::
  ( DefinitionStore :> es
  , ArchiveStore :> es
  , ScratchStore :> es
  , Time :> es
  ) =>
  User ->
  Eff es Response
homeContent user = do
  defaultScratch <- (\t -> Scratch (addDays 21 t) 0 0) <$> today
  scratch <- fromMaybe defaultScratch <$> Effects.ScratchStore.get user.email
  definitions <- Effects.DefinitionStore.getAll user.email
  archive <- Effects.ArchiveStore.getAll user.email
  let items = getItems scratch.endDate definitions archive
  pure $ htmlResponse $ Home.homeContent items scratch

getItems :: Day -> [Definition] -> [ArchivedItem] -> [Item]
getItems endDate definitions archivedItems =
  let items = definitions >>= takeWhile (\i -> i.itemDate <= endDate) . extractItems
   in sortOn itemDate $
        filter
          ( \item ->
              not $
                any
                  ( \archived ->
                      archived.archivedItemItemDefinitionId == item.itemDefinitionId
                        && archived.archivedItemDate == item.itemDate
                  )
                  archivedItems
          )
          items

extractItems :: Definition -> [Item]
extractItems definition =
  let dates = case definition.definitionFrequency of
        OneTime -> [definition.definitionStartDate]
        BiWeekly -> (\i -> addDays (i * 14) definition.definitionStartDate) <$> [0 ..]
        Monthly -> (\i -> addGregorianMonthsClip i definition.definitionStartDate) <$> [0 ..]
   in ( \d ->
          Item
            definition.definitionId
            definition.definitionDescription
            definition.definitionAmount
            d
            definition.definitionIsAutomaticWithdrawal
      )
        <$> dates
