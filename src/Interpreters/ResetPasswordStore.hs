module Interpreters.ResetPasswordStore where

import AppError
import Db.User qualified as UserDb
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader)
import Effects.ResetPasswordStore (ResetPasswordStore (..))
import Environment (Environment)
import Model (ExpirationTime (..))
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