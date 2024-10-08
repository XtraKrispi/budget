module Db where

import AppError (AppError)
import Control.Monad.Error.Class (MonadError)
import Environment (HasAppEnvironment, HasDbPath)
import Relude (MonadIO, MonadReader)

type WithDb env m =
  ( HasAppEnvironment env
  , HasDbPath env
  , MonadIO m
  , MonadReader env m
  , MonadError AppError m
  )