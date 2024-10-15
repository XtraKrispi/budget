{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.HashPassword where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import Model.Common
import Model.Password

data HashPassword :: Effect where
  HashPassword :: Password PlainText -> HashPassword m (Password Hashed)

makeEffect ''HashPassword