module Handlers.Home (getHome, postHome) where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, MonadTrans (lift))
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Time (Day, UTCTime (utctDay), addDays, addGregorianMonthsClip, getCurrentTime)
import Db qualified
import Environment (HasAppEnvironment, HasAuthCookieName, HasDbPath)
import Handlers.Global (errorToast)
import Html.Home qualified as Home
import Htmx.Request (isBoosted, isHtmx)
import Lucid (renderText)
import Model
import Web.Scotty.Auth (requiresAuth)
import Web.Scotty.Trans (ActionT, formParam, html)

getHome :: (HasAuthCookieName env, HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => ActionT m ()
getHome = requiresAuth \user -> do
  htmx <- isHtmx
  boosted <- isBoosted
  if not boosted && htmx
    then do
      homeContent user
    else html $ renderText $ Home.homePage user

postHome :: (HasAuthCookieName env, HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => ActionT m ()
postHome = requiresAuth \user -> do
  endDate <- unMyDay <$> formParam "end-date"
  amountInBank <- formParam "amount-in-bank"
  amountLeftOver <- formParam "amount-left-over"
  let newScratch = Scratch endDate amountInBank amountLeftOver
  _ <- lift $ Db.saveUserScratch user.email newScratch
  homeContent user
  pure ()

homeContent :: (HasAppEnvironment env, HasDbPath env, MonadReader env m, MonadIO m) => User -> ActionT m ()
homeContent user = do
  defaultScratch <- (\now -> Scratch (addDays 21 now) 0 0) . utctDay <$> liftIO getCurrentTime
  results <- lift $ runExceptT do
    scratch <- ExceptT $ Db.getScratch user.email
    definitions <- ExceptT $ Db.getAllDefinitions user.email
    archive <- ExceptT $ Db.getAllArchive user.email
    pure (fromMaybe defaultScratch scratch, definitions, archive)
  case results of
    Left _ -> errorToast "There was an issue fetching items, please refresh and try again."
    Right (scratch, defs, archive) -> do
      let items = getItems scratch.endDate defs archive
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
