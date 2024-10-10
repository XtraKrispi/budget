module Db where

import AppError (AppError)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Environment (HasAppEnvironment, HasDbPath)

type WithDb env m =
  ( HasAppEnvironment env
  , HasDbPath env
  , MonadIO m
  , MonadReader env m
  , MonadError AppError m
  )