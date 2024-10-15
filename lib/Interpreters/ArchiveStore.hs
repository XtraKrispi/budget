{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Interpreters.ArchiveStore where

import AppError (AppError)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Db.Archive qualified
import Effectful
import Effectful.Dispatch.Dynamic (interpret, reinterpret)
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local
import Effects.ArchiveStore (ArchiveStore (..))
import Environment
import Model.Archive
import Model.Email

runArchiveStoreSqlite ::
  ( IOE :> es
  , Reader Environment :> es
  , Error AppError :> es
  ) =>
  Eff (ArchiveStore : es) a ->
  Eff es a
runArchiveStoreSqlite = interpret \_ -> \case
  GetAll email -> Db.Archive.getAllArchive email
  Insert email item -> Db.Archive.insertArchive email item

runArchiveStorePure :: Map Email [ArchivedItem] -> Eff (ArchiveStore : es) a -> Eff es a
runArchiveStorePure initialStore = reinterpret (evalState initialStore) \_ -> \case
  GetAll email -> do
    gets (Map.lookup email) >>= \case
      Just items -> pure items
      Nothing -> pure []
  Insert email item -> modify $ Map.insertWith (++) email [item]
