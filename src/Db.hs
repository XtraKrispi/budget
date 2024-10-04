{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Db where

import Control.Exception (catch)
import Data.Text (pack)
import Data.Text.IO (hPutStrLn)
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple (Connection, Only (Only), execute, execute_, query, query_, setTrace, withConnection)
import Environment (DbPath (fromDbPath), Env (..), HasAppEnvironment (..), HasDbPath (..))
import Id
import Model (ArchivedItem (..), Definition (..), Email (..), Hashed, Password (unPassword), Scratch (..), SessionId (..), Token, User (..))
import MyUUID qualified
import Relude
import Text.RawString.QQ (r)

withTraceConnection :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => (Connection -> IO a) -> m a
withTraceConnection action = do
  dbPath' <- asks (fromDbPath . dbPath)
  appEnv <- asks appEnvironment
  liftIO $ withConnection dbPath' \conn -> do
    case appEnv of
      Dev -> do
        now <- getCurrentTime
        setTrace conn $ Just $ \t -> hPutStrLn stderr $ "SQLITE @ " <> pack (show now) <> ":  " <> t
      Prod -> setTrace conn Nothing
    action conn

runDb :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => (Connection -> IO a) -> m (Either Text a)
runDb cmd = withTraceConnection \conn -> do
  (pure <$> cmd conn) `catch` (\(e :: SomeException) -> pure $ Left $ pack $ show e)

initialize :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => m (Either Text ())
initialize =
  runDb \conn -> do
    let statements =
          [ "CREATE TABLE IF NOT EXISTS definitions(id INTEGER PRIMARY KEY AUTOINCREMENT, identifier TEXT, description TEXT, amount REAL, frequency TEXT, start_date TEXT, end_date TEXT NULL, is_automatic_withdrawal INTEGER, user_id INTEGER);"
          , "CREATE TABLE IF NOT EXISTS archive(id INTEGER PRIMARY KEY AUTOINCREMENT, identifier TEXT, item_definition_identifier TEXT, description TEXT, amount REAL, date TEXT, action_date TEXT, action TEXT, user_id INTEGER);"
          , "CREATE TABLE IF NOT EXISTS users(id INTEGER PRIMARY KEY AUTOINCREMENT, email TEXT, password_hash TEXT, name TEXT);"
          , "CREATE TABLE IF NOT EXISTS sessions(session_id TEXT PRIMARY KEY, user_id INTEGER, expiration_time TEXT);"
          , "CREATE TABLE IF NOT EXISTS scratch(id INTEGER PRIMARY KEY AUTOINCREMENT, user_id INTEGER UNIQUE, end_date TEXT, amount_in_bank REAL, amount_left_over REAL);"
          , "CREATE TABLE IF NOT EXISTS password_reset_tokens (user_id TEXT NOT NULL, token TEXT NOT NULL UNIQUE, token_expiry TEXT NOT NULL, PRIMARY KEY (user_id, token));"
          ]
    traverse_ (execute_ conn) statements

getUserByEmail :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => Email -> m (Either Text (Maybe User))
getUserByEmail email = do
  runDb $ \conn -> listToMaybe <$> query conn "SELECT email, name, password_hash FROM users WHERE email = ?" (Only email)

insertUser :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => User -> m (Either Text ())
insertUser user =
  runDb $ \conn -> execute conn "INSERT INTO users(email, name, password_hash) VALUES(?, ?, ?)" (user.email, user.name, unPassword user.passwordHash)

createSession :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => Email -> UTCTime -> m (Either Text SessionId)
createSession email expiry = do
  sessionId <- SessionId <$> liftIO MyUUID.nextRandom
  results <- runDb \conn ->
    execute
      conn
      [r|INSERT INTO sessions (session_id, user_id, expiration_time)
           SELECT ?, u.id, ?
           FROM users u
           WHERE email = ?;|]
      (sessionId, expiry, email)
  pure $ sessionId <$ results

getUserForSession :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => SessionId -> m (Either Text (User, UTCTime))
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
    Right [(email, name, passwordHash, expirationTime)] -> pure $ Right (User email name passwordHash, expirationTime)
    Right _ -> pure $ Left $ "Too many results getting user for session: " <> MyUUID.toText (unSessionId sessionId)
    Left err -> pure $ Left err

updateSession :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => SessionId -> UTCTime -> m (Either Text ())
updateSession sessionId expiration =
  runDb \conn ->
    execute
      conn
      [r|UPDATE sessions 
         SET expiration_time = ?
         WHERE session_id = ?;
      |]
      (expiration, sessionId)

deleteSession :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => SessionId -> m (Either Text ())
deleteSession sessionId =
  runDb \conn ->
    execute conn "DELETE FROM sessions WHERE session_id = ?;" (Only sessionId)

insertResetToken :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => Email -> Token Hashed -> UTCTime -> m (Either Text ())
insertResetToken email token expiry = do
  results :: Either Text [Only Text] <- runDb \conn ->
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
    Right [] -> pure $ Left "Not found"
    Right _ -> pure $ Right ()
    Left err -> pure $ Left err

getUsersForResetPassword :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => m (Either Text [(User, UTCTime, Token Hashed)])
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

updateUserPassword :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => Email -> Password Hashed -> m (Either Text ())
updateUserPassword email password =
  runDb \conn ->
    execute
      conn
      [r| UPDATE users 
          SET password_hash = ?
          WHERE email = ?;|]
      (password, email)

removeAllUserTokens :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => Email -> m (Either Text ())
removeAllUserTokens email = runDb \conn ->
  execute
    conn
    [r| DELETE FROM password_reset_tokens
        WHERE user_id IN 
        (SELECT id 
        FROM users 
        WHERE email = ?);|]
    (Only email)

getScratch :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => Email -> m (Either Text (Maybe Scratch))
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

getAllDefinitions :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => Email -> m (Either Text [Definition])
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

getAllArchive :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => Email -> m (Either Text [ArchivedItem])
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

saveUserScratch :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => Email -> Scratch -> m (Either Text ())
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
    (scratch.endDate, scratch.amountInBank, scratch.amountLeftOver, email)

insertArchive :: (HasAppEnvironment env, HasDbPath env, MonadIO m, MonadReader env m) => Email -> ArchivedItem -> m (Either Text ())
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

getDefinitionById ::
  ( HasAppEnvironment env
  , HasDbPath env
  , MonadIO m
  , MonadReader env m
  ) =>
  Id Definition ->
  m (Either Text (Maybe Definition))
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
  ( HasAppEnvironment env
  , HasDbPath env
  , MonadIO m
  , MonadReader env m
  ) =>
  Definition ->
  Email ->
  m (Either Text ())
upsertDefinition (Definition{..}) email = do
  mDef <- getDefinitionById definitionId
  case mDef of
    Left err -> pure $ Left err
    Right def -> do
      let sql = case def of
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
