module Model.User where

import Data.Text
import Database.SQLite.Simple (FromRow)
import GHC.Generics (Generic)
import Model.Common (Hashed)
import Model.Email (Email)
import Model.Password (Password)

data User = User
  { email :: Email
  , name :: Text
  , passwordHash :: Password Hashed
  }
  deriving (Generic, Show)
instance FromRow User