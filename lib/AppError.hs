module AppError where

import Control.Exception
import Data.Text

data AppError
  = DatabaseError Text
  | SessionError Text
  | UnknownError Text
  | MailError Text
  | BadRequest Text
  | NotFoundError
  deriving (Show)

instance Exception AppError
