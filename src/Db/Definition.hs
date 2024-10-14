{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Db.Definition where

import AppError (AppError)
import Data.Maybe (listToMaybe)
import Database.SQLite.Simple (Only (..), execute, query)
import Db.Internal
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static (Reader)
import Environment
import Model.Definition
import Model.Email
import Model.Id
import Text.RawString.QQ

getAllDefinitions ::
  ( IOE :> es
  , Reader Environment :> es
  , Error AppError :> es
  ) =>
  Email ->
  Eff es [Definition]
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
  ( IOE :> es
  , Reader Environment :> es
  , Error AppError :> es
  ) =>
  Email ->
  Id Definition ->
  Eff es (Maybe Definition)
getDefinitionById email defId = runDb \conn -> do
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
   FROM definitions d
   JOIN users u ON d.user_id = u.id
   WHERE identifier LIKE ? AND u.email = ?|]
      (defId, email)

upsertDefinition ::
  ( IOE :> es
  , Reader Environment :> es
  , Error AppError :> es
  ) =>
  Email ->
  Definition ->
  Eff es ()
upsertDefinition email (Definition{..}) = do
  mDef <- getDefinitionById email definitionId
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
