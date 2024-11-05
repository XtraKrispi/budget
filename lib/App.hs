module App where

import AppError
import Data.Function ((&))
import Data.Text (unpack)
import Effectful
import Effectful.Error.Static (CallStack, Error, runError)
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
  AppStaticDirectory (unAppStaticDirectory),
  Env (Dev),
  Environment (..),
 )
import Handlers qualified
import Handlers.Auth (requiresAuth)
import Handlers.Model
import Handlers.Utils (errorResponse, makeResponse)
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
import Model.Archive (ArchiveAction (..))
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Web.Scotty (ScottyM)
import Web.Scotty.ActionT (runHandler)
import Web.Scotty.Trans

runEffects ::
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
runEffects env program =
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
    Response ->
  IO Response
runProgram env program = do
  results <- runEffects env program
  case results of
    Left _err ->
      pure $ errorResponse "An unknown error has occurred, please try again."
    Right resp -> pure resp

appMiddleware :: Environment -> ScottyT m ()
appMiddleware environment = do
  middleware $ staticPolicy $ addBase $ unpack $ unAppStaticDirectory environment.envAppStaticDirectory
  middleware $
    if environment.envAppEnvironment == Dev
      then logStdoutDev
      else logStdout

webapp ::
  Environment -> ScottyM ()
webapp env = do
  get "/" $ runHandler (runProgram env) $ requiresAuth Handlers.getHome
  post "/" $ runHandler (runProgram env) $ requiresAuth Handlers.postHome
  get "/login" $ runHandler (runProgram env) $ const Handlers.getLogin
  post "/login" $ runHandler (runProgram env) Handlers.postLogin
  get "/toast/clear" $ runHandler (runProgram env) (const $ pure $ makeResponse [] [] mempty)
  post "/register" $ runHandler (runProgram env) Handlers.postRegister
  post "/register/validate" $ runHandler (runProgram env) Handlers.postRegisterValidate
  get "/reset-password" $ runHandler (runProgram env) $ const Handlers.getResetPassword
  post "/reset-password" $ runHandler (runProgram env) Handlers.postResetPassword
  get "/reset-password/:token" $ runHandler (runProgram env) Handlers.getResetPasswordToken
  post "/reset-password/validate" $ runHandler (runProgram env) Handlers.postResetPasswordValidate
  post "/reset-password/:token" $ runHandler (runProgram env) Handlers.postResetPasswordToken
  delete "/session" $ runHandler (runProgram env) $ requiresAuth (\r _ -> Handlers.deleteSession r)
  get "/archive" $ runHandler (runProgram env) $ requiresAuth Handlers.getArchive
  post "/archive/skip" $ runHandler (runProgram env) $ requiresAuth (Handlers.postArchiveAction Skipped)
  post "/archive/pay" $ runHandler (runProgram env) $ requiresAuth (Handlers.postArchiveAction Paid)
  get "/admin/definitions" $ runHandler (runProgram env) $ requiresAuth Handlers.getDefinitionPage
  get "/admin/definitions/new" $ runHandler (runProgram env) $ requiresAuth Handlers.getDefinitionEdit
  post "/admin/definitions/new" $ runHandler (runProgram env) $ requiresAuth Handlers.postDefinitionEdit
  get "/admin/definitions/:defId" $ runHandler (runProgram env) $ requiresAuth Handlers.getDefinitionEdit
  post "/admin/definitions/:defId" $ runHandler (runProgram env) $ requiresAuth Handlers.postDefinitionEdit