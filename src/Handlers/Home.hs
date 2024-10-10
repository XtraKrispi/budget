module Handlers.Home (getHome, postHome) where

import Control.Monad.Trans (lift)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Time (Day, addDays, addGregorianMonthsClip)
import Effectful
import Effects.ArchiveStore
import Effects.DefinitionStore
import Effects.ScratchStore
import Effects.Time
import Html.Home qualified as Home
import Htmx.Request (isBoosted, isHtmx)
import Model
import Web.Scotty.ActionT (renderHtml)
import Web.Scotty.Trans (ActionT, formParam)

getHome ::
  ( DefinitionStore :> es
  , Time :> es
  , ArchiveStore :> es
  , ScratchStore :> es
  , IOE :> es
  ) =>
  User ->
  ActionT (Eff es) ()
getHome user = do
  htmx <- isHtmx
  boosted <- isBoosted
  if not boosted && htmx
    then do
      homeContent user
    else renderHtml $ Home.homePage user

postHome ::
  ( DefinitionStore :> es
  , ArchiveStore :> es
  , ScratchStore :> es
  , Time :> es
  , IOE :> es
  ) =>
  User ->
  ActionT (Eff es) ()
postHome user = do
  endDate <- unMyDay <$> formParam "end-date"
  amountInBank <- formParam "amount-in-bank"
  amountLeftOver <- formParam "amount-left-over"

  let scratch = Scratch endDate amountInBank amountLeftOver
  lift $ Effects.ScratchStore.save user.email scratch
  homeContent user

homeContent ::
  ( DefinitionStore :> es
  , ArchiveStore :> es
  , ScratchStore :> es
  , Time :> es
  , IOE :> es
  ) =>
  User ->
  ActionT (Eff es) ()
homeContent user = do
  defaultScratch <- lift $ (\t -> Scratch (addDays 21 t) 0 0) <$> today
  scratch <- lift $ fromMaybe defaultScratch <$> Effects.ScratchStore.get user.email
  definitions <- lift $ Effects.DefinitionStore.getAll user.email
  archive <- lift $ Effects.ArchiveStore.getAll user.email
  let items = getItems scratch.endDate definitions archive
  renderHtml $ Home.homeContent items scratch

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
