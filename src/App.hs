module App where

import AppError (AppError (NotFoundError))
import Control.Exception (throwIO, try)
import Control.Monad.Error.Class
import Control.Monad.IO.Unlift
import Control.Monad.Random (MonadRandom)
import Data.Time (getCurrentTime)
import Db.Archive qualified as ArchiveDb
import Db.Definition qualified as DefinitionDb
import Db.Scratch qualified as ScratchDb
import Db.Session qualified as SessionDb
import Db.User qualified as UserDb
import Effects.Archive (MonadArchive (..))
import Effects.Config (MonadConfig, getBaseUrl)
import Effects.Definition
import Effects.Id
import Effects.Mail
import Effects.MyUUID
import Effects.Password
import Effects.ResetPassword (MonadResetPassword (..))
import Effects.Scratch (MonadScratch (..))
import Effects.Session
import Effects.Time
import Effects.User (MonadUser (..))
import Environment
import Id qualified
import Lucid
import Model
import MyUUID (nextRandom)
import Network.Mail.Mime (htmlPart)
import Network.Mail.SMTP (Address (..), sendMailWithLoginTLS, simpleMail)
import Password qualified
import Relude
import ResetPassword

newtype App a = App {unApp :: ReaderT Environment (ExceptT AppError IO) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Environment
    , MonadRandom
    , MonadError AppError
    )

instance MonadUnliftIO App where
  withRunInIO inner =
    App $ ReaderT $ \env ->
      ExceptT $ try $ do
        withRunInIO $ \runInIO ->
          inner (runInIO . (either throwIO pure <=< runApp env))

runApp :: Environment -> App a -> IO (Either AppError a)
runApp env = runExceptT . flip runReaderT env . unApp

instance MonadSession App where
  newSession :: Email -> ExpirationTime -> App SessionId
  newSession email (ExpirationTime ex) = SessionDb.createSession email ex

  slideSession :: SessionId -> ExpirationTime -> App ()
  slideSession sessionId (ExpirationTime ex) = SessionDb.updateSession sessionId ex

  getSessionUser :: SessionId -> App (Maybe (User, ExpirationTime))
  getSessionUser sessionId = do
    fmap (second ExpirationTime) <$> SessionDb.getUserForSession sessionId
  logout :: SessionId -> App ()
  logout = SessionDb.deleteSession

instance MonadTime App where
  now = liftIO getCurrentTime

instance MonadUser App where
  getUser = UserDb.getUserByEmail
  insert = UserDb.insertUser
  updatePassword = UserDb.updateUserPassword

instance MonadScratch App where
  get = ScratchDb.getScratch
  save = ScratchDb.saveUserScratch

instance MonadArchive App where
  insertArchive = ArchiveDb.insertArchive
  getAll = ArchiveDb.getAllArchive

instance MonadResetPassword App where
  generateToken = ResetPassword.generateToken
  insertToken email (ExpirationTime ex) token = do
    results <- UserDb.insertResetToken email token ex
    case results of
      Nothing -> throwError NotFoundError
      _ -> pure ()
  getUsers = fmap (fmap (\(user, ex, token) -> (user, ExpirationTime ex, token))) UserDb.getUsersForResetPassword
  removeUserTokens = UserDb.removeAllUserTokens

instance MonadDefinition App where
  getAll = DefinitionDb.getAllDefinitions
  getOne = DefinitionDb.getDefinitionById
  save = DefinitionDb.upsertDefinition

instance MonadMyUUID App where
  generate = MyUUID.nextRandom

instance MonadId App where
  generate = Id.newId

instance MonadMail App where
  sendMail to (Subject subject) body =
    do
      smtpConfig <- asks smtp
      let mail =
            simpleMail
              ( Address
                  (Just smtpConfig.smtpFromName)
                  smtpConfig.smtpFromEmail
              )
              [Address Nothing (unEmail to)]
              []
              []
              subject
              [htmlPart (renderText body)]
      liftIO $
        sendMailWithLoginTLS smtpConfig.smtpHostname smtpConfig.smtpUsername smtpConfig.smtpPassword mail

instance MonadConfig App where
  getBaseUrl = asks baseUrl

instance MonadPassword App where
  hashPassword = Password.hashPassword