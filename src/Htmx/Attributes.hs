module Htmx.Attributes where

import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.Text (encodeToLazyText)
import Lucid (Attributes)
import Lucid.Base (makeAttributes)
import Relude

hxBoost :: Attributes
hxBoost = makeAttributes "hx-boost" "true"

hxValidate :: Attributes
hxValidate = makeAttributes "hx-validate" "true"

hxDelete :: Text -> Attributes
hxDelete = makeAttributes "hx-delete"

hxSwapOob :: Text -> Attributes
hxSwapOob = makeAttributes "hx-swap-oob"

hxGet :: Text -> Attributes
hxGet = makeAttributes "hx-get"

hxPost :: Text -> Attributes
hxPost = makeAttributes "hx-post"

hxSwap :: Text -> Attributes
hxSwap = makeAttributes "hx-swap"

hxTrigger :: Text -> Attributes
hxTrigger = makeAttributes "hx-trigger"

hyper_ :: Text -> Attributes
hyper_ = makeAttributes "_"

hxTarget :: Text -> Attributes
hxTarget = makeAttributes "hx-target"

hxInclude :: Text -> Attributes
hxInclude = makeAttributes "hx-include"

hxVals :: (ToJSON a) => a -> Attributes
hxVals a = makeAttributes "hx-vals" (toStrict . encodeToLazyText $ toJSON a)

hxPushUrl :: Text -> Attributes
hxPushUrl = makeAttributes "hx-push-url"