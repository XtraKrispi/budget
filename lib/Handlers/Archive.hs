module Handlers.Archive where

import AppError
import Effectful
import Effectful.Error.Static (Error)
import Effects.ArchiveStore
import Effects.MakeMyUUID
import Effects.Time
import Handlers.Model
import Handlers.Utils (getParam, headerResponse, htmlResponse)
import Html.Archive qualified as Archive
import Htmx.Request (isBoosted, isHtmx)
import Model.Archive
import Model.Id
import Model.User

getArchive ::
  ( ArchiveStore :> es
  ) =>
  Request ->
  User ->
  Eff es Response
getArchive request user = do
  let htmx = isHtmx request
  let boosted = isBoosted request
  if htmx && not boosted
    then do
      items <- getAll user.email
      pure $ htmlResponse $ Archive.items items
    else do
      pure $ htmlResponse $ Archive.archivePage user

postArchiveAction ::
  ( MakeMyUUID :> es
  , Time :> es
  , ArchiveStore :> es
  , Error AppError :> es
  ) =>
  ArchiveAction ->
  Request ->
  User ->
  Eff es Response
postArchiveAction archivedItemAction request user = do
  archivedItemAmount <- getParam request "itemAmount"
  archivedItemDate <- getParam request "itemDate"
  archivedItemItemDefinitionId <- getParam request "itemDefinitionId"
  archivedItemDescription <- getParam request "itemDescription"

  archivedItemId <- Id <$> generate
  archivedItemActionDate <- today

  let item =
        ArchivedItem
          archivedItemId
          archivedItemItemDefinitionId
          archivedItemDescription
          archivedItemAmount
          archivedItemDate
          archivedItemActionDate
          archivedItemAction

  insert user.email item

  pure $ headerResponse "HX-Trigger" "reload"
