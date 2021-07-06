module Web.Controller.Home where

import Web.Controller.Prelude
import Web.View.Home.Index
import qualified Data.Map as Map
import Prelude (Traversable(traverse))

instance Controller HomeController where
  beforeAction = ensureIsUser
  action HomeAction = autoRefresh do
    today <- addDays 30 . utctDay <$> getCurrentTime 
    defs <- query @Definition |> filterWhere (#isDeleted, False) |> fetch 
    archive <- query @Archive |> fetch
    render $ IndexView $ sortOn (get #date) $ defs >>= getItems today
