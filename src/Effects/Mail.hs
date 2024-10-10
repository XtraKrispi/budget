{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.Mail where

import Data.Text (Text)
import Effectful
import Effectful.TH (makeEffect)
import Lucid
import Model (Email)

newtype Subject = Subject {unSubject :: Text}

data Mail :: Effect where
  SendMail :: Email -> Subject -> Html () -> Mail m ()

makeEffect ''Mail