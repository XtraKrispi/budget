{-# LANGUAGE QuasiQuotes #-}

module Db.Scratch where

import Data.Maybe (listToMaybe)
import Database.SQLite.Simple (Only (..), execute, query)
import Db.Internal
import Effectful
import Effectful.Reader.Static (Reader)
import Environment
import Model
import Text.RawString.QQ

getScratch :: (IOE :> es, Reader Environment :> es) => Email -> Eff es (Maybe Scratch)
getScratch email = runDb \conn -> do
  listToMaybe
    <$> query
      conn
      [r| SELECT s.end_date, s.amount_in_bank, s.amount_left_over
        FROM scratch s
        JOIN users u ON s.user_id = u.id
        WHERE u.email = ?
    |]
      (Only email)

saveUserScratch :: (IOE :> es, Reader Environment :> es) => Email -> Scratch -> Eff es ()
saveUserScratch email scratch = runDb \conn ->
  execute
    conn
    [r|
      INSERT INTO scratch(user_id, end_date, amount_in_bank, amount_left_over)
      SELECT u.id, ?, ?, ?
      FROM users u
      WHERE u.email = ?
      ON CONFLICT(user_id) DO
      UPDATE
      SET end_date = excluded.end_date
        , amount_in_bank = excluded.amount_in_bank
        , amount_left_over = excluded.amount_left_over;
  |]
    ( scratch.endDate
    , scratch.amountInBank
    , scratch.amountLeftOver
    , email
    )
