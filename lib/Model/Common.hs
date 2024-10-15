module Model.Common where

import Data.Time (UTCTime)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import Model.MyUUID (MyUUID)

newtype ExpirationTime = ExpirationTime {unExpirationTime :: UTCTime}

newtype SessionId = SessionId {unSessionId :: MyUUID}
  deriving (Show, Eq, Ord, ToField, FromField)

data PlainText
data Hashed
