module Interpreters.UserStore where

import AppError (AppError)
import Data.Foldable
import Db.User qualified as UserDb
import Effectful
import Effectful.Dispatch.Dynamic (interpret, reinterpret)
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (evalState, gets, modify)
import Effects.UserStore
import Environment
import Model.User

runUserStoreSqlite ::
  ( Reader Environment :> es
  , IOE :> es
  , Error AppError :> es
  ) =>
  Eff (UserStore : es) a ->
  Eff es a
runUserStoreSqlite = interpret \_ -> \case
  Get email -> UserDb.getUserByEmail email
  Insert user -> UserDb.insertUser user
  UpdatePassword email password -> UserDb.updateUserPassword email password

runUserStorePure :: [User] -> Eff (UserStore : es) a -> Eff es a
runUserStorePure initialStore = reinterpret (evalState initialStore) \_ -> \case
  Get email -> gets (\(s :: [User]) -> find (\user -> user.email == email) s)
  Insert user -> modify (\(s :: [User]) -> user : s)
  UpdatePassword email password ->
    modify
      ( \(s :: [User]) ->
          fmap
            ( \u ->
                if u.email == email
                  then
                    u{passwordHash = password}
                  else u
            )
            s
      )