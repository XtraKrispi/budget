{-# LANGUAGE QuasiQuotes #-}

module Db.User where

import AppError (AppError)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.SQLite.Simple (Only (..), execute, query, query_)
import Db.Internal
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static (Reader)
import Environment
import Model.Common
import Model.Email
import Model.Password
import Model.Token
import Model.User
import Text.RawString.QQ

getUserByEmail ::
  ( IOE :> es
  , Reader Environment :> es
  , Error AppError :> es
  ) =>
  Email ->
  Eff es (Maybe User)
getUserByEmail email = do
  runDb $ \conn -> listToMaybe <$> query conn "SELECT email, name, password_hash FROM users WHERE email = ?" (Only email)

insertUser ::
  ( IOE :> es
  , Reader Environment :> es
  , Error AppError :> es
  ) =>
  User ->
  Eff es ()
insertUser user =
  runDb $ \conn -> execute conn "INSERT INTO users(email, name, password_hash) VALUES(?, ?, ?)" (user.email, user.name, unPassword user.passwordHash)

insertResetToken ::
  ( IOE :> es
  , Reader Environment :> es
  , Error AppError :> es
  ) =>
  Email ->
  Token Hashed ->
  UTCTime ->
  Eff es (Maybe ())
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
    [] -> pure Nothing
    _ -> pure $ Just ()

getUsersForResetPassword ::
  ( IOE :> es
  , Reader Environment :> es
  , Error AppError :> es
  ) =>
  Eff es [(User, UTCTime, Token Hashed)]
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

updateUserPassword ::
  ( IOE :> es
  , Reader Environment :> es
  , Error AppError :> es
  ) =>
  Email ->
  Password Hashed ->
  Eff es ()
updateUserPassword email password =
  runDb \conn ->
    execute
      conn
      [r| UPDATE users 
          SET password_hash = ?
          WHERE email = ?;|]
      (password, email)

removeAllUserTokens ::
  ( IOE :> es
  , Reader Environment :> es
  , Error AppError :> es
  ) =>
  Email ->
  Eff es ()
removeAllUserTokens email = runDb \conn ->
  execute
    conn
    [r| DELETE FROM password_reset_tokens
        WHERE user_id IN 
        (SELECT id 
        FROM users 
        WHERE email = ?);|]
    (Only email)
