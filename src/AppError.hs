module AppError where

import Control.Monad.Error.Class (MonadError)
import Relude

data AppError
  = DatabaseError Text
  | SessionError Text
  | UnknownError Text
  | NotFoundError
  deriving (Show)

instance Exception AppError

type WithError m = MonadError AppError m