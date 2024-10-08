{-# LANGUAGE QuasiQuotes #-}

module Db.User where

import AppError (AppError (NotFoundError))
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Time (UTCTime)
import Database.SQLite.Simple (Only (..), execute, query, query_)
import Db
import Db.Internal
import Model
import Relude
import Text.RawString.QQ

getUserByEmail :: (WithDb env m) => Email -> m (Maybe User)
getUserByEmail email = do
  runDb $ \conn -> listToMaybe <$> query conn "SELECT email, name, password_hash FROM users WHERE email = ?" (Only email)

insertUser :: (WithDb env m) => User -> m ()
insertUser user =
  runDb $ \conn -> execute conn "INSERT INTO users(email, name, password_hash) VALUES(?, ?, ?)" (user.email, user.name, unPassword user.passwordHash)

insertResetToken :: (WithDb env m) => Email -> Token Hashed -> UTCTime -> m ()
insertResetToken email token expiry = do
  results :: [Only Text] <- runDb \conn ->
    query
      conn
      [r| 
      INSERT INTO password_reset_tokens(user_id, token, token_expiry)
      SELECT u.id, ?, ?
      FROM users u
      WHERE u.email = ?
      RETURNING user_id;|]
      (token, expiry, email)
  case results of
    [] -> throwError NotFoundError
    _ -> pure ()

getUsersForResetPassword :: (WithDb env m) => m [(User, UTCTime, Token Hashed)]
getUsersForResetPassword = do
  runDb \conn -> do
    results <-
      query_
        conn
        [r|  SELECT u.email
                  , u.password_hash
                  , u.name
                  , prt.token_expiry
                  , prt.token
             FROM password_reset_tokens prt
             JOIN users u ON prt.user_id = u.id;
        |]
    pure $
      ( \(email, passwordHash, name, expiry, token) ->
          (User email name passwordHash, expiry, token)
      )
        <$> results

updateUserPassword :: (WithDb env m) => Email -> Password Hashed -> m ()
updateUserPassword email password =
  runDb \conn ->
    execute
      conn
      [r| UPDATE users 
          SET password_hash = ?
          WHERE email = ?;|]
      (password, email)

removeAllUserTokens :: (WithDb env m) => Email -> m ()
removeAllUserTokens email = runDb \conn ->
  execute
    conn
    [r| DELETE FROM password_reset_tokens
        WHERE user_id IN 
        (SELECT id 
        FROM users 
        WHERE email = ?);|]
    (Only email)
