module Handlers.Model where

import Data.Text (Text, unpack)
import Data.Time (DiffTime)
import Lucid
import Text.Read (readMaybe)

class Parse a where
  parse :: Text -> Maybe a

instance Parse Double where
  parse :: Text -> Maybe Double
  parse = readMaybe . unpack

instance Parse Text where
  parse :: Text -> Maybe Text
  parse = pure

type Header = (Text, Text)

data Cookie = Cookie
  { cookieName :: Text
  , cookieValue :: Text
  , cookieMaxAge :: Maybe DiffTime
  , cookieHttpOnly :: Bool
  , cookieSecure :: Bool
  }

data Request = Request
  { requestCookies :: [(Text, Text)]
  , requestHeaders :: [Header]
  , requestParams :: [(Text, Text)]
  }

data SamePageResponse = SamePageResponse
  { samePageResponseHeaders :: [Header]
  , samePageResponseCookies :: [Cookie]
  , samePageResponseContent :: Html ()
  }

newtype RedirectResponse = RedirectResponse {redirectResponseUrl :: Text}

data Response
  = Redirect RedirectResponse
  | SamePage SamePageResponse
