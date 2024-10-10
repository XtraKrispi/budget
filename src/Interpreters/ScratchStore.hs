module Interpreters.ScratchStore where

import Db.Scratch qualified as ScratchDb
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Reader.Static (Reader)
import Effects.ScratchStore
import Environment

runScratchStoreSqlite :: (Reader Environment :> es, IOE :> es) => Eff (ScratchStore : es) a -> Eff es a
runScratchStoreSqlite = interpret \_ -> \case
  Get email -> ScratchDb.getScratch email
  Save email scratch -> ScratchDb.saveUserScratch email scratch