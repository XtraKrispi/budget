{-# LANGUAGE QuasiQuotes #-}

module Db.Session where

import Data.Time (UTCTime)
import Database.SQLite.Simple (Only (..), execute, query)
import Db.Internal
import Effectful
import Effectful.Reader.Static (Reader)
import Environment
import Model
import MyUUID qualified
import Text.RawString.QQ

createSession :: (IOE :> es, Reader Environment :> es) => Email -> UTCTime -> Eff es SessionId
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

getUserForSession :: (IOE :> es, Reader Environment :> es) => SessionId -> Eff es (Maybe (User, UTCTime))
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

updateSession :: (IOE :> es, Reader Environment :> es) => SessionId -> UTCTime -> Eff es ()
updateSession sessionId expiration =
  runDb \conn ->
    execute
      conn
      [r|UPDATE sessions 
         SET expiration_time = ?
         WHERE session_id = ?;
      |]
      (expiration, sessionId)

deleteSession :: (IOE :> es, Reader Environment :> es) => SessionId -> Eff es ()
deleteSession sessionId =
  runDb \conn ->
    execute conn "DELETE FROM sessions WHERE session_id = ?;" (Only sessionId)
