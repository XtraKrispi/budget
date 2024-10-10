{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Interpreters.DefinitionStore where

import Data.Foldable (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Db.Definition qualified
import Effectful
import Effectful.Dispatch.Dynamic (interpret, reinterpret)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (evalState, gets, modify)
import Effects.DefinitionStore
import Environment
import Model

runDefinitionStoreSqlite :: (IOE :> es, Reader Environment :> es) => Eff (DefinitionStore : es) a -> Eff es a
runDefinitionStoreSqlite = interpret \_ -> \case
  GetAll email -> Db.Definition.getAllDefinitions email
  Get email definitionId -> Db.Definition.getDefinitionById email definitionId
  Save email definition -> Db.Definition.upsertDefinition email definition

runDefinitionStorePure :: Map Email [Definition] -> Eff (DefinitionStore : es) a -> Eff es a
runDefinitionStorePure initialStore = reinterpret (evalState initialStore) \_ -> \case
  GetAll email ->
    gets (Map.lookup email) >>= \case
      Just items -> pure items
      Nothing -> pure []
  Get email definitionId -> do
    items :: Maybe [Definition] <- gets (Map.lookup email)
    pure $ items >>= find (\def -> def.definitionId == definitionId)
  Save email definition ->
    modify
      ( \(m :: Map Email [Definition]) ->
          case Map.lookup email m of
            Just defs ->
              Map.insert
                email
                ( foldr
                    ( \def ds ->
                        if def.definitionId == definition.definitionId
                          then
                            ds
                          else def : ds
                    )
                    [definition]
                    defs
                )
                m
            Nothing -> Map.insert email [definition] m
      )
