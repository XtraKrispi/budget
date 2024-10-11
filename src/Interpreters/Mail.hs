module Interpreters.Mail where

import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Reader.Static (Reader, asks)
import Effects.Mail
import Environment (Environment (envSmtp), Smtp (..))
import Lucid
import Model (Email (..))
import Network.Mail.Mime (Address (..), htmlPart)
import Network.Mail.SMTP (sendMailWithLoginTLS, simpleMail)

runMailIO :: (IOE :> es, Reader Environment :> es) => Eff (Mail : es) a -> Eff es a
runMailIO = interpret \_ -> \case
  SendMail to (Subject subject) body -> do
    smtpConfig <- asks envSmtp
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
    liftIO $ sendMailWithLoginTLS smtpConfig.smtpHostname smtpConfig.smtpUsername smtpConfig.smtpPassword mail