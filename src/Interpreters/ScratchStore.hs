module Interpreters.ScratchStore where

import AppError (AppError)
import Data.Map (Map)
import Data.Map qualified as Map
import Db.Scratch qualified as ScratchDb
import Effectful
import Effectful.Dispatch.Dynamic (interpret, reinterpret)
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local
import Effects.ScratchStore
import Environment
import Model

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

runScratchStorePure :: Map Email Scratch -> Eff (ScratchStore : es) a -> Eff es a
runScratchStorePure initialStore = reinterpret (evalState initialStore) \_ -> \case
  Get email -> gets (Map.lookup email)
  Save email scratch -> modify (Map.insert email scratch)
