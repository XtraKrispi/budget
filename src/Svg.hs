module Svg where

import Lucid (Attributes, Term (term))
import Lucid.Base (makeAttributes)
import Relude

path_ :: (Term arg result) => arg -> result
path_ = term "path"

fill_ :: Text -> Attributes
fill_ = makeAttributes "fill"

viewBox_ :: Text -> Attributes
viewBox_ = makeAttributes "viewBox"

stroke_ :: Text -> Attributes
stroke_ = makeAttributes "stroke"

strokeLinecap_ :: Text -> Attributes
strokeLinecap_ = makeAttributes "stroke-linecap"

strokeLinejoin_ :: Text -> Attributes
strokeLinejoin_ = makeAttributes "stroke-linejoin"

strokeWidth_ :: Text -> Attributes
strokeWidth_ = makeAttributes "stroke-width"

d_ :: Text -> Attributes
d_ = makeAttributes "d"