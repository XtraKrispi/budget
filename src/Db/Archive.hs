{-# LANGUAGE QuasiQuotes #-}

module Db.Archive where

import Database.SQLite.Simple (Only (..), execute, query)
import Db
import Db.Internal
import Model
import Text.RawString.QQ

getAllArchive ::
  (WithDb env m) =>
  Email ->
  m [ArchivedItem]
getAllArchive email = runDb \conn ->
  query
    conn
    [r| SELECT  identifier
              , item_definition_identifier
              , description
              , amount
              , date
              , action_date
              , action
        FROM archive a
        JOIN users u ON a.user_id = u.id
        WHERE u.email = ?    
    |]
    (Only email)

insertArchive ::
  (WithDb env m) =>
  Email ->
  ArchivedItem ->
  m ()
insertArchive email archive = runDb \conn ->
  execute
    conn
    [r| INSERT INTO archive(identifier
        , item_definition_identifier
        , description
        , amount
        , date
        , action_date
        , action
        , user_id)
        SELECT ?, ?, ?, ?, ?, ?, ?, u.id
        FROM users u
        WHERE u.email = ?;|]
    ( archive.archivedItemId
    , archive.archivedItemItemDefinitionId
    , archive.archivedItemDescription
    , archive.archivedItemAmount
    , archive.archivedItemDate
    , archive.archivedItemActionDate
    , archive.archivedItemAction
    , email
    )
