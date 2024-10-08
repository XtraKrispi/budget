{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Db.Definition where

import Database.SQLite.Simple (Only (..), execute, query)
import Db
import Db.Internal
import Id
import Model
import Relude
import Text.RawString.QQ

getAllDefinitions :: (WithDb env m) => Email -> m [Definition]
getAllDefinitions email = runDb \conn ->
  query
    conn
    [r| SELECT  d.identifier
              , d.description
              , d.amount
              , d.frequency
              , d.start_date
              , d.end_date
              , d.is_automatic_withdrawal
        FROM definitions d
        JOIN users u ON d.user_id = u.id
        WHERE u.email = ?
    |]
    (Only email)

getDefinitionById ::
  (WithDb env m) =>
  Id Definition ->
  m (Maybe Definition)
getDefinitionById defId = runDb \conn -> do
  listToMaybe
    <$> query
      conn
      [r|SELECT identifier
        , description
        , amount
        , frequency
        , start_date
        , end_date
        , is_automatic_withdrawal
   FROM definitions
   WHERE identifier LIKE ?|]
      (Only defId)

upsertDefinition ::
  (WithDb env m) => Email -> Definition -> m ()
upsertDefinition email (Definition{..}) = do
  mDef <- getDefinitionById definitionId
  let sql = case mDef of
        Just _ ->
          [r| UPDATE definitions
                  SET description = ?
                    ,amount = ?
                    ,frequency = ?
                    ,start_date = ?
                    ,end_date = ?
                    ,is_automatic_withdrawal = ?
                  WHERE identifier LIKE ? AND user_id = (SELECT u.id FROM users u WHERE u.email = ?);|]
        Nothing ->
          [r| INSERT INTO definitions(description, amount, frequency, start_date, end_date, is_automatic_withdrawal, identifier, user_id)
                  SELECT ?,?, ?, ?, ?, ?, ?, u.id
                  FROM users u
                  WHERE email = ?;|]
  runDb \conn -> do
    execute
      conn
      sql
      ( definitionDescription
      , definitionAmount
      , definitionFrequency
      , definitionStartDate
      , definitionEndDate
      , definitionIsAutomaticWithdrawal
      , definitionId
      , email
      )
