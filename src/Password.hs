module Password where

import Data.Password.Argon2 qualified as Argon2
import Model (Hashed, Password (Password), PlainText)
import Relude

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