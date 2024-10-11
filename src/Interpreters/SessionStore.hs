module Interpreters.SessionStore where

import AppError (AppError)
import Data.Bifunctor (second)
import Data.Map (Map, elems, insert)
import Data.Map qualified as Map
import Data.UUID (fromWords)
import Data.Word (Word32)
import Db.Session qualified as SessionDb
import Effectful
import Effectful.Dispatch.Dynamic (interpret, reinterpret)
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local
import Effects.SessionStore (SessionStore (..))
import Environment
import Model (Email (unEmail), ExpirationTime (..), Password (..), SessionId (..), User (..))
import MyUUID (MyUUID (MyUUID))

runSessionStoreSqlite ::
  ( IOE :> es
  , Reader Environment :> es
  , Error AppError :> es
  ) =>
  Eff (SessionStore : es) a ->
  Eff es a
runSessionStoreSqlite = interpret \_ -> \case
  NewSession email (ExpirationTime expirationTime) -> SessionDb.createSession email expirationTime
  SlideSession sessionId (ExpirationTime expirationTime) -> SessionDb.updateSession sessionId expirationTime
  GetSessionUser sessionId -> fmap (second ExpirationTime) <$> SessionDb.getUserForSession sessionId
  Logout sessionId -> SessionDb.deleteSession sessionId

runSessionStorePure :: Map SessionId (Word32, Email, ExpirationTime) -> Eff (SessionStore : es) a -> Eff es a
runSessionStorePure initialStore = reinterpret (evalState initialStore) \_ -> \case
  NewSession email e -> do
    allValues :: [(Word32, Email, ExpirationTime)] <- gets (\(s :: Map SessionId (Word32, Email, ExpirationTime)) -> elems s)
    let maxSession = maximum $ fmap (\(i, _, _) -> i) allValues
    let newSession = maxSession + 1
    let newSessionId = SessionId $ MyUUID $ fromWords 0 0 0 newSession
    modify (insert newSessionId (newSession, email, e))
    pure newSessionId
  SlideSession sessionId ex ->
    modify
      ( Map.update
          ( \((i, e, _) :: (Word32, Email, ExpirationTime)) ->
              Just (i, e, ex)
          )
          sessionId
      )
  GetSessionUser sessionId -> do
    looked :: Maybe (Word32, Email, ExpirationTime) <- gets (Map.lookup sessionId)
    case looked of
      Just (_, email, ex) -> pure $ Just (User email (unEmail email) (Password ""), ex)
      Nothing -> pure Nothing
  Logout sessionId -> modify (\(s :: Map SessionId (Word32, Email, ExpirationTime)) -> Map.delete sessionId s)
