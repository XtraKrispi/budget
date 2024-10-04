module Html.Home where

import Data.Text (pack)
import Data.Text.Format.Numbers
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatShow)
import Html.Common (withLayout)
import Htmx.Attributes
import Lucid
import Model (Item (..), Scratch (..), User)
import Relude

homePage :: User -> Html ()
homePage user = withLayout user $ div_ [class_ "px-20 py-10"] do
  header_ [class_ "prose lg:prose-xl flex space-x-4"] do
    h2_ "Home"
  div_
    [ id_ "home-content"
    , hxTrigger "load"
    , hxGet "/"
    , hxSwap "outerHTML"
    ]
    do
      main_ "Loading"
      aside_ "Loading"

homeContent :: [Item] -> Scratch -> Html ()
homeContent items scratch =
  div_
    [ class_ "flex space-x-12 w-screen"
    , id_ "home-content"
    , hxTrigger "reload from:body"
    , hxGet "/"
    , hxSwap "outerHTML"
    ]
    do
      itemsSection items
      scratchArea (sum $ fmap itemAmount items) scratch

itemsSection :: [Item] -> Html ()
itemsSection items = main_ [class_ "flex flex-col space-y-4"] do
  if null items
    then
      div_ "No items to display!"
    else
      traverse_ renderItem items

renderItem :: Item -> Html ()
renderItem item = div_ [class_ "card w-96 bg-base-100 shadow-xl", hxVals item] do
  div_ [class_ "card-body"] do
    h2_ [class_ "card-title flex justify-between"] do
      span_ (toHtml item.itemDescription)
      span_ [class_ "cursor-pointer", hyper_ ("on click writeText('" <> prettyF (PrettyCfg 2 Nothing '.') item.itemAmount <> "') on navigator.clipboard")] do
        toHtml $ "$" <> prettyF (PrettyCfg 2 (Just ',') '.') item.itemAmount
    p_ $ toHtml $ formatShow iso8601Format item.itemDate
    div_ [class_ "flex justify-between items-center"] do
      div_ do
        when item.itemIsAutomaticWithdrawal do
          div_ [class_ "badge badge-primary"] "Automatic"
      div_ [class_ "card-actions justify-end"] do
        button_ [class_ "btn", hxPost "/archive/skip"] "Skip"
        button_ [class_ "btn btn-primary", hxPost "/archive/pay"] "Pay"

scratchArea :: Double -> Scratch -> Html ()
scratchArea total scratch =
  aside_
    [class_ "prose flex flex-col space-y-4 min-w-[320px]"]
    do
      form_
        [ class_ "flex flex-col space-y-4"
        , hxPost "/"
        , hxTarget "#home-content"
        , hxSwap "outerHTML"
        , hxPushUrl "true"
        ]
        do
          input_
            [ class_ "input input-bordered w-full max-w-xs"
            , value_ $ pack $ formatShow iso8601Format scratch.endDate
            , name_ "end-date"
            , type_ "date"
            ]
          input_
            [ class_ "input input-bordered w-full max-w-xs"
            , placeholder_ "Amount in account"
            , name_ "amount-in-bank"
            , value_ $
                if scratch.amountInBank == 0
                  then ""
                  else
                    prettyF (PrettyCfg 2 Nothing '.') scratch.amountInBank
            ]
          input_
            [ class_ "input input-bordered w-full max-w-xs"
            , placeholder_ "Amount to be left over"
            , name_ "amount-left-over"
            , value_ $
                if scratch.amountLeftOver == 0
                  then ""
                  else
                    prettyF (PrettyCfg 2 Nothing '.') scratch.amountLeftOver
            ]
          button_ [class_ "btn btn-primary", type_ "submit"] "Recalculate"
      div_ do
        h3_ [class_ "flex justify-between"] do
          span_ "Total Owing: "
          span_ $ toHtml $ "$" <> prettyF (PrettyCfg 2 (Just ',') '.') total
        h3_ [class_ "flex justify-between"] do
          span_ "Total Outstanding: "
          span_ $ toHtml $ "$" <> prettyF (PrettyCfg 2 (Just ',') '.') (max 0 (total + scratch.amountLeftOver - scratch.amountInBank))
