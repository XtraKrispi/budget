module Handlers.Home (getHome, postHome) where

import Data.Time (Day, UTCTime (utctDay), addDays, addGregorianMonthsClip, getCurrentTime)
import Effects.Archive (MonadArchive)
import Effects.Archive qualified
import Effects.Definition (MonadDefinition)
import Effects.Definition qualified
import Effects.Scratch
import Html.Home qualified as Home
import Htmx.Request (isBoosted, isHtmx)
import Lucid (renderText)
import Model
import Relude
import Web.Scotty.Trans (ActionT, formParam, html)

getHome ::
  ( MonadIO m
  , MonadDefinition m
  , MonadArchive m
  , MonadScratch m
  ) =>
  User ->
  ActionT m ()
getHome user = do
  htmx <- isHtmx
  boosted <- isBoosted
  if not boosted && htmx
    then do
      homeContent user
    else html $ renderText $ Home.homePage user

postHome ::
  ( MonadIO m
  , MonadDefinition m
  , MonadArchive m
  , MonadScratch m
  ) =>
  User ->
  ActionT m ()
postHome user = do
  endDate <- unMyDay <$> formParam "end-date"
  amountInBank <- formParam "amount-in-bank"
  amountLeftOver <- formParam "amount-left-over"
  let newScratch = Scratch endDate amountInBank amountLeftOver
  Effects.Scratch.save user.email newScratch
  homeContent user

homeContent ::
  ( MonadIO m
  , MonadDefinition m
  , MonadArchive m
  , MonadScratch m
  ) =>
  User ->
  ActionT m ()
homeContent user = do
  defaultScratch <- (\now -> Scratch (addDays 21 now) 0 0) . utctDay <$> liftIO getCurrentTime
  scratch <- fromMaybe defaultScratch <$> Effects.Scratch.get user.email
  definitions <- Effects.Definition.getAll user.email
  archive <- Effects.Archive.getAll user.email
  let items = getItems scratch.endDate definitions archive
  html $ renderText $ Home.homeContent items scratch

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
