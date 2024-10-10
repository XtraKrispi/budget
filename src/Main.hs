module Main (main) where

import AppError
import Auth (requiresAuth)
import Configuration.Dotenv qualified as Dotenv
import Control.Monad.Identity (Identity (..))
import Data.Function ((&))
import Data.Text (pack)
import Data.Text.IO qualified as TIO
import Db.Init qualified as InitDb
import Effectful
import Effectful.Error.Static (Error, runError)
import Effectful.Reader.Static (Reader, runReader)
import Effects.ArchiveStore (ArchiveStore)
import Effects.DefinitionStore (DefinitionStore)
import Effects.HashPassword (HashPassword)
import Effects.Mail (Mail)
import Effects.MakeId (MakeId)
import Effects.MakeMyUUID (MakeMyUUID)
import Effects.ResetPasswordStore (ResetPasswordStore)
import Effects.ScratchStore (ScratchStore)
import Effects.SessionStore (SessionStore)
import Effects.Time (Time)
import Effects.UserStore (UserStore)
import Environment (
  Env (Dev),
  Environment (envAppEnvironment),
 )
import GHC.Exception (CallStack)
import GHC.IO.StdHandles (stderr)
import Handlers qualified
import Html.Common (addToast)
import Interpreters.ArchiveStore (runArchiveStoreSqlite)
import Interpreters.DefinitionStore (runDefinitionStoreSqlite)
import Interpreters.HashPassword (runHashPasswordIO)
import Interpreters.Mail (runMailIO)
import Interpreters.MakeId (runMakeIdIO)
import Interpreters.MakeMyUUID (runMakeMyUUIDIO)
import Interpreters.ResetPasswordStore (runResetPasswordStoreSqlite)
import Interpreters.ScratchStore (runScratchStoreSqlite)
import Interpreters.SessionStore (runSessionStoreSqlite)
import Interpreters.Time (runTimeIO)
import Interpreters.UserStore (runUserStoreSqlite)
import Lucid
import Model (AlertType (..), ArchiveAction (..))
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (responseLBS)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
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
      results <- runEff $ runReader environment $ runError @AppError InitDb.initialize
      case results of
        Left (_callstack, err) -> TIO.putStrLn $ "There was a problem initializing the database: " <> pack (show err)
        Right _ -> pure ()
      scottyT
        8080
        ( \app -> do
            r <- runProgram environment app
            case r of
              Right resp -> pure resp
              Left _err -> do
                pure $ responseLBS status200 [(hContentType, "text/html; charset=utf-8"), ("HX-Reswap", "none")] $ runIdentity $ renderBST $ addToast Error (span_ "There was an error processing the request. Please try again.")
        )
        (appMiddleware environment >> webapp)

runProgram ::
  (Reader Environment :> [Reader r, Error AppError, IOE]) =>
  r ->
  Eff
    [ ArchiveStore
    , DefinitionStore
    , ResetPasswordStore
    , ScratchStore
    , SessionStore
    , UserStore
    , HashPassword
    , Time
    , Mail
    , MakeId
    , MakeMyUUID
    , Reader r
    , Error AppError
    , IOE
    ]
    a ->
  IO (Either (CallStack, AppError) a)
runProgram env program =
  program
    & runArchiveStoreSqlite
    & runDefinitionStoreSqlite
    & runResetPasswordStoreSqlite
    & runScratchStoreSqlite
    & runSessionStoreSqlite
    & runUserStoreSqlite
    & runHashPasswordIO
    & runTimeIO
    & runMailIO
    & runMakeIdIO
    & runMakeMyUUIDIO
    & runReader env
    & runError @AppError
    & runEff

appMiddleware :: Environment -> ScottyT m ()
appMiddleware environment = do
  middleware $ staticPolicy (addBase "public")
  middleware $
    if environment.envAppEnvironment == Dev
      then logStdoutDev
      else logStdout

webapp ::
  ( SessionStore :> es
  , Time :> es
  , UserStore :> es
  , ScratchStore :> es
  , ArchiveStore :> es
  , ResetPasswordStore :> es
  , DefinitionStore :> es
  , MakeMyUUID :> es
  , HashPassword :> es
  , Reader Environment :> es
  , IOE :> es
  , Mail :> es
  , MakeId :> es
  ) =>
  ScottyT (Eff es) ()
webapp = do
  get "/" $ requiresAuth Handlers.getHome
  post "/" $ requiresAuth Handlers.postHome
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
  delete "/session" $ requiresAuth Handlers.deleteSession
  get "/archive" $ requiresAuth Handlers.getArchive
  post "/archive/skip" $ requiresAuth (Handlers.postArchiveAction Skipped)
  post "/archive/pay" $ requiresAuth (Handlers.postArchiveAction Paid)
  get "/admin/definitions" $ requiresAuth Handlers.getDefinitionPage
  get "/admin/definitions/new" $ requiresAuth Handlers.getDefinitionEdit
  post "/admin/definitions/new" $ requiresAuth Handlers.postDefinitionEdit
  get "/admin/definitions/:defId" $ requiresAuth Handlers.getDefinitionEdit
  post "/admin/definitions/:defId" $ requiresAuth Handlers.postDefinitionEdit