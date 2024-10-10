module Interpreters.UserStore where

import Db.User qualified as UserDb
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Reader.Static (Reader)
import Effects.UserStore
import Environment

runUserStoreSqlite :: (Reader Environment :> es, IOE :> es) => Eff (UserStore : es) a -> Eff es a
runUserStoreSqlite = interpret \_ -> \case
  Get email -> UserDb.getUserByEmail email
  Insert user -> UserDb.insertUser user
  UpdatePassword email password -> UserDb.updateUserPassword email password