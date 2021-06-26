module Web.Controller.Archive where

import Web.Controller.Prelude
import Web.View.Archive.Index
import Web.View.Archive.New
import Web.View.Archive.Edit
import Web.View.Archive.Show

instance Controller ArchiveController where
    action ArchivesAction = do
        archive <- query @Archive |> fetch
        render IndexView { .. }

    action NewArchiveAction = do
        let archive = newRecord
        render NewView { .. }

    action ShowArchiveAction { archiveId } = do
        archive <- fetch archiveId
        render ShowView { .. }

    action EditArchiveAction { archiveId } = do
        archive <- fetch archiveId
        render EditView { .. }

    action UpdateArchiveAction { archiveId } = do
        archive <- fetch archiveId
        archive
            |> buildArchive
            |> ifValid \case
                Left archive -> render EditView { .. }
                Right archive -> do
                    archive <- archive |> updateRecord
                    setSuccessMessage "Archive updated"
                    redirectTo EditArchiveAction { .. }

    action CreateArchiveAction = do
        let archive = newRecord @Archive
        archive
            |> buildArchive
            |> ifValid \case
                Left archive -> render NewView { .. } 
                Right archive -> do
                    archive <- archive |> createRecord
                    setSuccessMessage "Archive created"
                    redirectTo ArchivesAction

    action DeleteArchiveAction { archiveId } = do
        archive <- fetch archiveId
        deleteRecord archive
        setSuccessMessage "Archive deleted"
        redirectTo ArchivesAction

buildArchive archive = archive
    |> fill @["definitionId","description","amount","date"]
