module Main (main) where

import Configuration.Dotenv qualified as Dotenv
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Random (MonadRandom)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Data.Text.IO qualified as TIO
import Db qualified
import Environment (Env (Dev), Environment (envAppEnvironment), HasAppEnvironment, HasAuthCookieName, HasBaseUrl, HasDbPath, HasSmtp)
import Handlers qualified
import Model (ArchiveAction (..))
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import System.Envy (decodeEnv)
import System.IO (hPutStrLn, stderr)
import Web.Scotty.Trans

main :: IO ()
main = do
  Dotenv.onMissingFile (Dotenv.loadFile Dotenv.defaultConfig) (pure ())
  eEnvironment <- decodeEnv
  case eEnvironment of
    Left err -> do
      hPutStrLn stderr "Cannot start application, environment variables not set!"
      hPutStrLn stderr err
    Right environment -> do
      results <- runReaderT Db.initialize environment
      case results of
        Left err -> TIO.putStrLn $ "There was a problem initializing the database: " <> err
        Right _ -> pure ()
      scottyT 8080 (`runReaderT` environment) (appMiddleware environment >> webapp)

appMiddleware :: Environment -> ScottyT m ()
appMiddleware environment = do
  middleware $ staticPolicy (addBase "public")
  middleware $
    if environment.envAppEnvironment == Dev
      then logStdoutDev
      else logStdout

webapp ::
  ( HasSmtp env
  , HasBaseUrl env
  , HasAppEnvironment env
  , HasAuthCookieName env
  , HasDbPath env
  , MonadUnliftIO m
  , MonadReader env m
  , MonadRandom m
  ) =>
  ScottyT m ()
webapp = do
  get "/" Handlers.getHome
  post "/" Handlers.postHome
  get "/login" Handlers.getLogin
  post "/login" Handlers.postLogin
  get "/toast/clear" Handlers.clearToast
  post "/register" Handlers.postRegister
  post "/register/validate" Handlers.postRegisterValidate
  get "/reset-password" Handlers.getResetPassword
  post "/reset-password" Handlers.postResetPassword
  get "/reset-password/:token" Handlers.getResetPasswordToken
  post "/reset-password/validate" Handlers.postResetPasswordValidate
  post "/reset-password/:token" Handlers.postResetPasswordToken
  delete "/session" Handlers.deleteSession
  get "/archive" Handlers.getArchive
  post "/archive/skip" (Handlers.postArchiveAction Skipped)
  post "/archive/pay" (Handlers.postArchiveAction Paid)
  get "/admin/definitions" Handlers.getDefinitionPage
  get "/admin/definitions/new" Handlers.getDefinitionEdit
  post "/admin/definitions/new" Handlers.postDefinitionEdit
  get "/admin/definitions/:defId" Handlers.getDefinitionEdit
  post "/admin/definitions/:defId" Handlers.postDefinitionEdit