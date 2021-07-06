module Application.Helper.Controller where

import IHP.ControllerPrelude
import Generated.Types
import Web.Types

-- Here you can add functions which are available in all your controllers

getDates :: Frequency -> Day -> [Day]
getDates Onetime d = [d]
getDates Weekly d = (\n -> addDays (n * 7) d) <$> [0,1..]
getDates Biweekly d = (\n -> addDays (n * 14) d) <$> [0,1..]
getDates Monthly d = (`addGregorianMonthsClip` d) <$> [0,1..]

getItems :: Day -> Definition -> [BudgetItem]
getItems day Definition{..} = takeWhile (\i -> get #date i <= day)  $ BudgetItem id description amount <$> getDates frequency startDate
