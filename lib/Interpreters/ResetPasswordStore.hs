module Interpreters.ResetPasswordStore where

import AppError
import Control.Monad.Random (MonadRandom (..))
import Data.Password.Argon2 qualified as Argon2
import Data.Text (pack)
import Db.User qualified as UserDb
import Effectful
import Effectful.Dispatch.Dynamic (interpret, reinterpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (evalState, get, modify)
import Effects.ResetPasswordStore (ResetPasswordStore (..))
import Environment (Environment)
import Model.Common
import Model.Email
import Model.Password
import Model.Token
import Model.User (User (..))

runResetPasswordStoreSqlite :: (IOE :> es, Reader Environment :> es, Error AppError :> es) => Eff (ResetPasswordStore : es) a -> Eff es a
runResetPasswordStoreSqlite = interpret \_ -> \case
  GenerateToken -> liftIO do
    str <- pack . take 64 <$> getRandomRs ('=', 'z')
    hashed <- Token . Argon2.unPasswordHash <$> Argon2.hashPassword (Argon2.mkPassword str)
    pure (Token str, hashed)
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