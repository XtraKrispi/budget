module Interpreters.ScratchStore where

import AppError (AppError)
import Db.Scratch qualified as ScratchDb
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader)
import Effects.ScratchStore
import Environment

runScratchStoreSqlite ::
  ( Reader Environment :> es
  , IOE :> es
  , Error AppError :> es
  ) =>
  Eff (ScratchStore : es) a ->
  Eff es a
runScratchStoreSqlite = interpret \_ -> \case
  Get email -> ScratchDb.getScratch email
  Save email scratch -> ScratchDb.saveUserScratch email scratch