module Handlers.Home (getHome, postHome) where

import Data.Time (Day, addDays, addGregorianMonthsClip)
import Effects.Archive (MonadArchive)
import Effects.Archive qualified
import Effects.Definition (MonadDefinition)
import Effects.Definition qualified
import Effects.Scratch
import Effects.Time
import Effects.WebServer (MonadWebServer (fromForm, serveHtml))
import Handlers.Global (errorToast)
import Html.Home qualified as Home
import Htmx.Request (isBoosted, isHtmx)
import Model
import Relude

getHome ::
  ( MonadWebServer m
  , MonadDefinition m
  , MonadArchive m
  , MonadScratch m
  , MonadTime m
  ) =>
  User ->
  m ()
getHome user = do
  htmx <- isHtmx
  boosted <- isBoosted
  if not boosted && htmx
    then do
      homeContent user
    else serveHtml $ Home.homePage user

postHome ::
  ( MonadWebServer m
  , MonadDefinition m
  , MonadArchive m
  , MonadScratch m
  , MonadTime m
  ) =>
  User ->
  m ()
postHome user = do
  endDate <- fmap unMyDay <$> fromForm "end-date"
  amountInBank <- fromForm "amount-in-bank"
  amountLeftOver <- fromForm "amount-left-over"

  let newScratch = Scratch <$> endDate <*> amountInBank <*> amountLeftOver
  case newScratch of
    Just scratch -> do
      Effects.Scratch.save user.email scratch
      homeContent user
    Nothing -> errorToast "There was an issue with your request, please try again."

homeContent ::
  ( MonadWebServer m
  , MonadDefinition m
  , MonadArchive m
  , MonadScratch m
  , MonadTime m
  ) =>
  User ->
  m ()
homeContent user = do
  defaultScratch <- (\t -> Scratch (addDays 21 t) 0 0) <$> today
  scratch <- fromMaybe defaultScratch <$> Effects.Scratch.get user.email
  definitions <- Effects.Definition.getAll user.email
  archive <- Effects.Archive.getAll user.email
  let items = getItems scratch.endDate definitions archive
  serveHtml $ Home.homeContent items scratch

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
