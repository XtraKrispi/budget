module Interpreters.ResetPasswordStore where

import AppError
import Db.User qualified as UserDb
import Effectful
import Effectful.Dispatch.Dynamic (interpret, reinterpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (evalState, get, modify)
import Effects.ResetPasswordStore (ResetPasswordStore (..))
import Environment (Environment)
import Model (Email (unEmail), ExpirationTime (..), Hashed, Password (..), Token (..), User (..))
import ResetPassword qualified

runResetPasswordStoreSqlite :: (IOE :> es, Reader Environment :> es, Error AppError :> es) => Eff (ResetPasswordStore : es) a -> Eff es a
runResetPasswordStoreSqlite = interpret \_ -> \case
  GenerateToken -> liftIO ResetPassword.generateToken
  InsertToken email (ExpirationTime ex) token -> do
    results <- UserDb.insertResetToken email token ex
    case results of
      Nothing -> throwError NotFoundError
      _ -> pure ()
  GetUsers -> fmap (fmap (\(user, ex, token) -> (user, ExpirationTime ex, token))) UserDb.getUsersForResetPassword
  RemoveUserTokens email -> UserDb.removeAllUserTokens email

runResetPasswordStorePure :: [(Email, ExpirationTime, Token Hashed)] -> Eff (ResetPasswordStore : es) a -> Eff es a
runResetPasswordStorePure initialStore = reinterpret (evalState initialStore) \_ -> \case
  GenerateToken -> pure (Token "this is a new token", Token "this is a token hashed")
  InsertToken email expiration token -> do
    modify (\s -> (email, expiration, token) : s)
  GetUsers -> do
    fmap
      ( \(email, e, token) ->
          ( User
              { email = email
              , name = unEmail email
              , passwordHash = Password ""
              }
          , e
          , token
          )
      )
      <$> get
  RemoveUserTokens email ->
    modify (filter (\((e, _, _) :: (Email, ExpirationTime, Token Hashed)) -> e /= email))