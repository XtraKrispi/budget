module Environment where

import Data.Maybe (fromMaybe)
import Data.Text
import GHC.Generics
import System.Envy

defaultEnvOptions :: Option
defaultEnvOptions = Option{dropPrefixCount = 0, customPrefix = "BUDGET"}

data Smtp = Smtp
  { smtpRelay :: !String
  , smtpHostname :: !String
  , smtpFromName :: !Text
  , smtpFromEmail :: !Text
  , smtpUsername :: !String
  , smtpPassword :: !String
  }
  deriving (Generic, Show)

instance FromEnv Smtp where
  fromEnv :: Maybe Smtp -> Parser Smtp
  fromEnv = gFromEnvCustom defaultEnvOptions

newtype BaseUrl = BaseUrl {fromBaseUrl :: Text}
  deriving (Generic, Show)

instance FromEnv BaseUrl where
  fromEnv :: Maybe BaseUrl -> Parser BaseUrl
  fromEnv mUrl =
    flip fromMaybe mUrl . BaseUrl
      <$> envMaybe "BUDGET_BASE_URL"
        .!= "http://localhost:8080"

newtype DbPath = DbPath {fromDbPath :: String}
  deriving (Show)

instance DefConfig DbPath where
  defConfig :: DbPath
  defConfig = DbPath "budget.db"

data Env = Dev | Prod
  deriving (Show, Eq)

instance FromEnv Env where
  fromEnv :: Maybe Env -> Parser Env
  fromEnv mEnv =
    case mEnv of
      Just e -> pure e
      Nothing ->
        do
          var :: Maybe Text <- envMaybe "BUDGET_ENVIRONMENT"
          case var of
            Just "prod" -> pure Prod
            _ -> pure Dev

instance DefConfig Env where
  defConfig :: Env
  defConfig = Dev

data Environment = Environment
  { envDbPath :: !DbPath
  , envAuthCookieName :: !Text
  , envSmtp :: !Smtp
  , envBaseUrl :: !BaseUrl
  , envAppEnvironment :: !Env
  }
  deriving (Generic, Show)

instance FromEnv Environment where
  fromEnv :: Maybe Environment -> Parser Environment
  fromEnv mEnv = Environment defConfig "AUTH_COOKIE" <$> fromEnv (envSmtp <$> mEnv) <*> fromEnv (envBaseUrl <$> mEnv) <*> fromEnv (envAppEnvironment <$> mEnv)

class HasSmtp env where
  smtp :: env -> Smtp

class HasAppEnvironment env where
  appEnvironment :: env -> Env

class HasDbPath env where
  dbPath :: env -> DbPath

class HasBaseUrl env where
  baseUrl :: env -> BaseUrl

class HasAuthCookieName env where
  authCookieName :: env -> Text

instance HasSmtp Environment where
  smtp :: Environment -> Smtp
  smtp = envSmtp

instance HasAppEnvironment Environment where
  appEnvironment :: Environment -> Env
  appEnvironment = envAppEnvironment

instance HasDbPath Environment where
  dbPath :: Environment -> DbPath
  dbPath = envDbPath

instance HasBaseUrl Environment where
  baseUrl :: Environment -> BaseUrl
  baseUrl = envBaseUrl

instance HasAuthCookieName Environment where
  authCookieName :: Environment -> Text
  authCookieName = envAuthCookieName