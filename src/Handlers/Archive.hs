module Handlers.Archive where

import Control.Monad.Trans (lift)
import Effectful
import Effects.ArchiveStore
import Effects.MakeMyUUID
import Effects.Time
import Html.Archive qualified as Archive
import Htmx.Request (isBoosted, isHtmx)
import Id
import Model (ArchiveAction, ArchivedItem (..), MyDay (unMyDay), User (email))
import Web.Scotty.ActionT (renderHtml)
import Web.Scotty.Trans (ActionT, formParam, setHeader)

getArchive ::
  ( ArchiveStore :> es
  , IOE :> es
  ) =>
  User ->
  ActionT (Eff es) ()
getArchive user = do
  htmx <- isHtmx
  boosted <- isBoosted
  if htmx && not boosted
    then do
      items <- lift $ getAll user.email
      renderHtml $ Archive.items items
    else do
      renderHtml $ Archive.archivePage user

postArchiveAction ::
  ( MakeMyUUID :> es
  , Time :> es
  , ArchiveStore :> es
  , IOE :> es
  ) =>
  ArchiveAction ->
  User ->
  ActionT (Eff es) ()
postArchiveAction archivedItemAction user = do
  archivedItemAmount <- formParam "itemAmount"
  archivedItemDate <- unMyDay <$> formParam "itemDate"
  archivedItemItemDefinitionId <- formParam "itemDefinitionId"
  archivedItemDescription <- formParam "itemDescription"

  archivedItemId <- lift $ Id <$> generate
  archivedItemActionDate <- lift today

  let item =
        ArchivedItem
          archivedItemId
          archivedItemItemDefinitionId
          archivedItemDescription
          archivedItemAmount
          archivedItemDate
          archivedItemActionDate
          archivedItemAction

  lift $ insert user.email item

  setHeader "HX-Trigger" "reload"
