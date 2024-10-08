{-# LANGUAGE RecordWildCards #-}

module Handlers.Archive where

import Effects.Archive
import Effects.MyUUID (MonadMyUUID (generate))
import Effects.Time (MonadTime (..))
import Html.Archive qualified as Archive
import Htmx.Request (isBoosted, isHtmx)
import Id
import Lucid
import Model (ArchiveAction, ArchivedItem (..), MyDay (unMyDay), User (email))
import Relude
import Web.Scotty.Trans (ActionT, formParam, html, setHeader)

getArchive ::
  ( MonadArchive m
  , MonadIO m
  ) =>
  User ->
  ActionT m ()
getArchive user = do
  htmx <- isHtmx
  boosted <- isBoosted
  if htmx && not boosted
    then do
      items <- Effects.Archive.getAll user.email
      html $ renderText $ Archive.items items
    else do
      html $ renderText $ Archive.archivePage user

postArchiveAction ::
  ( MonadIO m
  , MonadMyUUID m
  , MonadTime m
  , MonadArchive m
  ) =>
  ArchiveAction ->
  User ->
  ActionT m ()
postArchiveAction archivedItemAction user = do
  archivedItemAmount <- formParam "itemAmount"
  archivedItemDate <- unMyDay <$> formParam "itemDate"
  archivedItemItemDefinitionId <- formParam "itemDefinitionId"
  archivedItemDescription <- formParam "itemDescription"

  archivedItemId <- Id <$> generate
  archivedItemActionDate <- today

  let item = ArchivedItem{..}

  insertArchive user.email item

  setHeader "HX-Trigger" "reload"
