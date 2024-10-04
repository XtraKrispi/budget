{-# LANGUAGE RecordWildCards #-}

module Handlers.Archive where

import Data.Time (UTCTime (utctDay), getCurrentTime)
import Db qualified
import Environment (HasAppEnvironment, HasAuthCookieName, HasDbPath)
import Handlers.Global (errorToast)
import Html.Archive qualified as Archive
import Htmx.Request (isBoosted, isHtmx)
import Id
import Lucid
import Model (ArchiveAction, ArchivedItem (..), MyDay (unMyDay), User (email))
import MyUUID qualified
import Relude
import Web.Scotty.Auth (requiresAuth)
import Web.Scotty.Trans (ActionT, formParam, html, setHeader)

getArchive ::
  ( HasAuthCookieName env
  , HasAppEnvironment env
  , HasDbPath env
  , MonadIO m
  , MonadReader env m
  ) =>
  ActionT m ()
getArchive = requiresAuth \user -> do
  htmx <- isHtmx
  boosted <- isBoosted
  if htmx && not boosted
    then do
      archiveItems <- lift $ Db.getAllArchive user.email
      case archiveItems of
        Right items -> html $ renderText $ Archive.items items
        Left _ -> errorToast "There was a problem retrieving archive items. Please refresh and try again."
      pure ()
    else do
      html $ renderText $ Archive.archivePage user

postArchiveAction ::
  ( HasAuthCookieName env
  , HasAppEnvironment env
  , HasDbPath env
  , MonadIO m
  , MonadReader env m
  ) =>
  ArchiveAction ->
  ActionT m ()
postArchiveAction archivedItemAction = requiresAuth \user -> do
  archivedItemAmount <- formParam "itemAmount"
  archivedItemDate <- unMyDay <$> formParam "itemDate"
  archivedItemItemDefinitionId <- formParam "itemDefinitionId"
  archivedItemDescription <- formParam "itemDescription"

  archivedItemId <- liftIO $ Id <$> MyUUID.nextRandom
  archivedItemActionDate <- liftIO $ utctDay <$> getCurrentTime

  let item = ArchivedItem{..}

  results <- lift $ Db.insertArchive user.email item

  either
    (const $ errorToast "Could not move to the archive, please try again.")
    (const $ setHeader "HX-Trigger" "reload")
    results
