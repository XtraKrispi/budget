module Web.Controller.Home where

import Web.Controller.Prelude
import Web.View.Home.Index

instance Controller HomeController where
  beforeAction = ensureIsUser
  action HomeAction = do
    render IndexView
