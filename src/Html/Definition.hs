{-# LANGUAGE QuasiQuotes #-}

module Html.Definition where

import Data.Foldable (traverse_)
import Data.Text (Text, pack)
import Data.Text.Format.Numbers (PrettyCfg (..), prettyF)
import Data.Time.Format.ISO8601 (ISO8601 (..), formatShow)
import Html.Common (withLayout)
import Html.Dialog qualified as Dialog
import Htmx.Attributes (hxGet, hxPost, hxSwap, hxTarget, hxTrigger, hyper_)
import Id
import Lucid
import Model (Definition (..), Frequency (..), User)
import Svg (d_, fill_, path_, strokeLinecap_, strokeLinejoin_, strokeWidth_, stroke_, viewBox_)
import Text.RawString.QQ

definitionsPage :: User -> Html ()
definitionsPage user = withLayout user do
  div_
    [ class_ "px-20 py-10"
    , hyper_
        [r| on showDefinitionsModal call definition_modal.showModal() end
            on hideDefinitionsModal call definition_modal.close() end
        |]
    ]
    do
      header_ [class_ "prose lg:prose-xl flex space-x-4"] do
        h2_ "Definitions"
        button_
          [ class_ "btn btn-primary"
          , hxGet "/admin/definitions/new"
          , hxTarget "#definition_modal"
          , hxSwap "outerHTML"
          ]
          do
            "New"
            svg_
              [ class_ "h-6 w-6"
              , fill_ "none"
              , viewBox_ "0 0 24 24"
              , strokeWidth_ "1.5"
              , stroke_ "currentColor"
              ]
              do
                path_
                  [ strokeLinecap_ "round"
                  , strokeLinejoin_ "round"
                  , strokeWidth_ "2"
                  , d_ "M5 12h14m-7 7V5"
                  ]
                  ""
      main_
        [ id_ "definitions"
        , hxGet "/admin/definitions"
        , hxTrigger "load"
        , hxSwap "outerHTML"
        ]
        "Loading"
      definitionsModal Nothing

definitions :: [Definition] -> Html ()
definitions defs =
  main_
    [ id_ "definitions"
    , hxGet "/admin/definitions"
    , hxSwap "outerHTML"
    , hxTrigger "reload from:body"
    ]
    do
      if null defs
        then
          p_ "No definitions found"
        else table_ [class_ "table table-zebra"] do
          thead_ do
            tr_ do
              th_ "Description"
              th_ "Amount"
              th_ "Frequency"
              th_ "Start Date"
              th_ "End Date"
          tbody_ do
            traverse_ renderDefinitionRow defs
 where
  renderDefinitionRow :: Definition -> Html ()
  renderDefinitionRow definition =
    tr_
      [ class_ "hover cursor-pointer"
      , hxGet ("/admin/definitions/" <> Id.toText definition.definitionId)
      , hxTarget "#definition_modal"
      , hxSwap "outerHTML"
      ]
      do
        th_ [class_ "flex justify-between"] do
          span_ (toHtml definition.definitionDescription)
          if definition.definitionIsAutomaticWithdrawal
            then
              div_ [class_ "badge badge-primary"] "Automatic"
            else mempty
        td_ (toHtml $ "$" <> prettyF (PrettyCfg 2 (Just ',') '.') definition.definitionAmount)
        td_ (toHtml $ prettyFrequency definition.definitionFrequency)
        td_ (toHtml $ formatShow iso8601Format definition.definitionStartDate)
        td_ (toHtml $ maybe "--" (formatShow iso8601Format) definition.definitionEndDate)

definitionsModal :: Maybe Definition -> Html ()
definitionsModal mDef = do
  dialog_ [class_ "modal", id_ "definition_modal"] do
    case mDef of
      Nothing -> pure ()
      Just def -> do
        div_ [class_ "modal-box"] do
          Dialog.closeButton
          h3_ [class_ "font-bold text-lg"] "Edit Definition"
          form_
            [ class_ "flex flex-col space-y-2"
            , hxPost ("/admin/definitions/" <> Id.toText def.definitionId)
            ]
            do
              input_
                [ type_ "hidden"
                , name_ "id"
                , value_ (Id.toText def.definitionId)
                ]
              label_ [class_ "form-control w-full max-w-xs"] do
                input_
                  [ class_ "input input-bordered w-full max-w-xs"
                  , value_ def.definitionDescription
                  , placeholder_ "Description"
                  , name_ "description"
                  , required_ "required"
                  ]
              div_ [class_ "form-control w-full max-w-xs"] do
                label_ [class_ "label cursor-pointer"] do
                  span_ [class_ "label-text"] "Is Automatic?"
                  input_
                    [ type_ "checkbox"
                    , name_ "is-automatic-withdrawal"
                    , class_ "checkbox"
                    , if def.definitionIsAutomaticWithdrawal then checked_ else mempty
                    ]
              input_
                [ class_ "input input-bordered w-full max-w-xs"
                , placeholder_ "Amount"
                , name_ "amount"
                , required_ "required"
                , value_ (prettyF (PrettyCfg 2 Nothing '.') def.definitionAmount)
                ]
              select_ [class_ "select select-bordered w-full max-w-xs", name_ "frequency"] do
                traverse_
                  ( \f ->
                      option_
                        [ if f == def.definitionFrequency then selected_ "selected" else mempty
                        , value_ (pack $ show f)
                        ]
                        do
                          toHtml (prettyFrequency f)
                  )
                  [OneTime .. Monthly]
              input_
                [ class_ "input input-bordered w-full max-w-xs"
                , name_ "start-date"
                , type_ "date"
                , required_ "required"
                , value_ $ pack $ formatShow iso8601Format def.definitionStartDate
                ]
              input_
                [ class_ "input input-bordered w-full max-w-xs"
                , name_ "end-date"
                , type_ "date"
                , value_ $ maybe "" (pack . formatShow iso8601Format) def.definitionEndDate
                ]
              div_ [class_ "modal-action"] do
                button_ [class_ "btn btn-primary", type_ "submit"] do
                  "Save"
                  svg_
                    [ class_ "h-6 w-6"
                    , fill_ "none"
                    , viewBox_ "0 0 24 24"
                    ]
                    do
                      path_
                        [ stroke_ "currentColor"
                        , strokeLinecap_ "round"
                        , strokeLinejoin_ "round"
                        , strokeWidth_ "2"
                        , d_ "M11 16h2m6.707-9.293-2.414-2.414A1 1 0 0 0 16.586 4H5a1 1 0 0 0-1 1v14a1 1 0 0 0 1 1h14a1 1 0 0 0 1-1V7.414a1 1 0 0 0-.293-.707ZM16 20v-6a1 1 0 0 0-1-1H9a1 1 0 0 0-1 1v6h8ZM9 4h6v3a1 1 0 0 1-1 1h-4a1 1 0 0 1-1-1V4Z"
                        ]
                        mempty
        form_
          [ method_ "dialog"
          , class_ "modal-backdrop"
          ]
          do
            button_ "close"

prettyFrequency :: Frequency -> Text
prettyFrequency OneTime = "One Time"
prettyFrequency BiWeekly = "Bi-Weekly"
prettyFrequency Monthly = "Monthly"
