module AppError where

import Relude

data AppError
  = DatabaseError Text
  | SessionError Text
  | UnknownError Text
  | NotFoundError
  deriving (Show)

instance Exception AppError
