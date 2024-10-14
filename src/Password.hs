module Password where

import Control.Monad.IO.Class (MonadIO)
import Data.Password.Argon2 qualified as Argon2
import Model.Common (Hashed, PlainText)
import Model.Password (Password (..))

hashPassword :: (MonadIO m) => Password PlainText -> m (Password Hashed)
hashPassword (Password password) =
  Password . Argon2.unPasswordHash <$> Argon2.hashPassword (Argon2.mkPassword password)

validatePassword :: Password Hashed -> Password PlainText -> Bool
validatePassword (Password hashedPassword) (Password plainTextPassword) =
  let results =
        Argon2.checkPassword
          (Argon2.mkPassword plainTextPassword)
          (Argon2.PasswordHash hashedPassword)
   in results == Argon2.PasswordCheckSuccess