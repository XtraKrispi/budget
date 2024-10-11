module Interpreters.UserStore where

import AppError (AppError)
import Db.User qualified as UserDb
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader)
import Effects.UserStore
import Environment

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