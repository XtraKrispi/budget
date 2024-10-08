{-# LANGUAGE QuasiQuotes #-}

module Db.Session where

import Data.Time (UTCTime)
import Database.SQLite.Simple (Only (..), execute, query)
import Db
import Db.Internal
import Model
import MyUUID qualified
import Relude
import Text.RawString.QQ

createSession :: (WithDb env m) => Email -> UTCTime -> m SessionId
createSession email expiry = do
  sessionId <- SessionId <$> liftIO MyUUID.nextRandom
  runDb \conn ->
    execute
      conn
      [r|INSERT INTO sessions (session_id, user_id, expiration_time)
           SELECT ?, u.id, ?
           FROM users u
           WHERE email = ?;|]
      (sessionId, expiry, email)
  pure sessionId

getUserForSession :: (WithDb env m) => SessionId -> m (Maybe (User, UTCTime))
getUserForSession sessionId = do
  results <- runDb \conn -> do
    query
      conn
      [r| 
      SELECT u.email
        , u.name
        , u.password_hash
        , s.expiration_time
      FROM users u 
      JOIN sessions s ON u.id = s.user_id
      WHERE s.session_id = ?|]
      (Only sessionId)

  case results of
    [(email, name, passwordHash, expirationTime)] -> pure $ Just (User email name passwordHash, expirationTime)
    _ -> pure Nothing

updateSession :: (WithDb env m) => SessionId -> UTCTime -> m ()
updateSession sessionId expiration =
  runDb \conn ->
    execute
      conn
      [r|UPDATE sessions 
         SET expiration_time = ?
         WHERE session_id = ?;
      |]
      (expiration, sessionId)

deleteSession :: (WithDb env m) => SessionId -> m ()
deleteSession sessionId =
  runDb \conn ->
    execute conn "DELETE FROM sessions WHERE session_id = ?;" (Only sessionId)
