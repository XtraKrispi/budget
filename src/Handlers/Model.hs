module Handlers.Model where

import Data.Text (Text, unpack)
import Data.Time (DiffTime)
import Data.Time.Calendar (Day)
import Data.Time.Format.ISO8601 (iso8601ParseM)
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

instance Parse Day where
  parse :: Text -> Maybe Day
  parse date = iso8601ParseM (unpack date)

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
