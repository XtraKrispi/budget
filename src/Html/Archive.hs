module Html.Archive where

import Data.Text.Format.Numbers (PrettyCfg (..), prettyF)
import Data.Time.Format.ISO8601 (ISO8601 (..), formatShow)
import Html.Common (withLayout)
import Htmx.Attributes (hxGet, hxSwap, hxTrigger)
import Lucid
import Model (ArchiveAction (..), ArchivedItem (..), User)
import Relude

archivePage :: User -> Html ()
archivePage user = withLayout user $ div_ [class_ "px-20 py-10"] do
  header_ [class_ "prose lg:prose-xl flex space-x-4"] do
    h2_ "Archive"
  main_ [hxSwap "outerHTML", hxGet "/archive", hxTrigger "load"] do
    "Loading"

items :: [ArchivedItem] -> Html ()
items xs =
  main_ [class_ "prose flex flex-col space-y-4"] do
    if null xs
      then
        h3_ "No items found..."
      else
        traverse_ renderItem xs

renderItem :: ArchivedItem -> Html ()
renderItem item = div_ [class_ "card w-96 bg-base-100 shadow-xl"] do
  div_ [class_ "card-body"] do
    h2_ [class_ "card-title flex justify-between"] do
      span_ (toHtml item.archivedItemDescription)
      span_ (toHtml $ "$" <> prettyF (PrettyCfg 2 (Just ',') '.') item.archivedItemAmount)
    p_ $ toHtml $ formatShow iso8601Format item.archivedItemDate
    div_ [class_ "flex justify-between items-center"] do
      div_ do
        case item.archivedItemAction of
          Paid -> div_ [class_ "badge badge-primary"] "Paid"
          Skipped -> div_ [class_ "badge badge-accent"] "Skipped"
