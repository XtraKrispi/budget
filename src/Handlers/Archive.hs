module Handlers.Archive where

import Effects.Archive
import Effects.MyUUID (MonadMyUUID (generate))
import Effects.Time (MonadTime (..))
import Effects.WebServer (MonadWebServer (..))
import Handlers.Global (errorToast)
import Html.Archive qualified as Archive
import Htmx.Request (isBoosted, isHtmx)
import Id
import Model (ArchiveAction, ArchivedItem (..), MyDay (unMyDay), User (email))
import Relude

getArchive ::
  ( MonadArchive m
  , MonadWebServer m
  ) =>
  User ->
  m ()
getArchive user = do
  htmx <- isHtmx
  boosted <- isBoosted
  if htmx && not boosted
    then do
      items <- Effects.Archive.getAll user.email
      serveHtml $ Archive.items items
    else do
      serveHtml $ Archive.archivePage user

postArchiveAction ::
  ( MonadIO m
  , MonadMyUUID m
  , MonadTime m
  , MonadArchive m
  , MonadWebServer m
  ) =>
  ArchiveAction ->
  User ->
  m ()
postArchiveAction archivedItemAction user = do
  mArchivedItemAmount <- fromForm "itemAmount"
  mArchivedItemDate <- fmap unMyDay <$> fromForm "itemDate"
  mArchivedItemItemDefinitionId <- fromForm "itemDefinitionId"
  mArchivedItemDescription <- fromForm "itemDescription"

  archivedItemId <- Id <$> generate
  archivedItemActionDate <- today

  let item =
        ArchivedItem archivedItemId
          <$> mArchivedItemItemDefinitionId
          <*> mArchivedItemDescription
          <*> mArchivedItemAmount
          <*> mArchivedItemDate
          <*> pure archivedItemActionDate
          <*> pure archivedItemAction

  case item of
    Just i -> do
      insertArchive user.email i

      setResponseHeader "HX-Trigger" "reload"
    Nothing ->
      errorToast "There was an issue with the request, please try again."
