module Main (main) where

import App (App, runApp)
import Auth (requiresAuth)
import Configuration.Dotenv qualified as Dotenv
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text (pack)
import Data.Text.IO qualified as TIO
import Db.Init qualified as InitDb
import Effects.Archive (MonadArchive)
import Effects.Config
import Effects.Definition (MonadDefinition)
import Effects.Id
import Effects.Mail
import Effects.MyUUID (MonadMyUUID)
import Effects.Password
import Effects.ResetPassword (MonadResetPassword)
import Effects.Scratch (MonadScratch)
import Effects.Session (MonadSession)
import Effects.Time (MonadTime)
import Effects.User
import Environment (
  Env (Dev),
  Environment (envAppEnvironment),
  HasAuthCookieName,
 )
import Handlers qualified
import Html.Common (addToast)
import Lucid
import Model (AlertType (..), ArchiveAction (..))
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (responseLBS)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Relude hiding (get)
import System.Envy (decodeEnv)
import System.IO (hPutStrLn)
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
      results <- runApp environment InitDb.initialize
      case results of
        Left err -> TIO.putStrLn $ "There was a problem initializing the database: " <> pack (show err)
        Right _ -> pure ()
      scottyT
        8080
        ( \app -> do
            r <- runApp environment app
            case r of
              Right resp -> pure resp
              Left _err -> do
                pure $ responseLBS status200 [(hContentType, "text/html; charset=utf-8"), ("HX-Reswap", "none")] $ runIdentity $ renderBST $ addToast Error (span_ "There was an error processing the request. Please try again.")
        )
        (appMiddleware environment >> webapp)

appMiddleware :: Environment -> ScottyT App ()
appMiddleware environment = do
  middleware $ staticPolicy (addBase "public")
  middleware $
    if environment.envAppEnvironment == Dev
      then logStdoutDev
      else logStdout

webapp ::
  ( MonadUnliftIO m
  , MonadSession m
  , MonadTime m
  , MonadUser m
  , MonadScratch m
  , MonadArchive m
  , MonadResetPassword m
  , MonadDefinition m
  , MonadMyUUID m
  , MonadPassword m
  , MonadConfig m
  , MonadMail m
  , MonadId m
  , MonadReader env m
  , HasAuthCookieName env
  ) =>
  ScottyT m ()
webapp = do
  get "/" (requiresAuth Handlers.getHome)
  post "/" (requiresAuth Handlers.postHome)
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
  delete "/session" (requiresAuth Handlers.deleteSession)
  get "/archive" (requiresAuth Handlers.getArchive)
  post "/archive/skip" (requiresAuth (Handlers.postArchiveAction Skipped))
  post "/archive/pay" (requiresAuth (Handlers.postArchiveAction Paid))
  get "/admin/definitions" (requiresAuth Handlers.getDefinitionPage)
  get "/admin/definitions/new" (requiresAuth Handlers.getDefinitionEdit)
  post "/admin/definitions/new" (requiresAuth Handlers.postDefinitionEdit)
  get "/admin/definitions/:defId" (requiresAuth Handlers.getDefinitionEdit)
  post "/admin/definitions/:defId" (requiresAuth Handlers.postDefinitionEdit)