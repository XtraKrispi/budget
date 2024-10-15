module ResetPassword where

import Data.Foldable (find)
import Data.Password.Argon2 qualified as Argon2
import Data.Time (UTCTime)
import Model.Common (ExpirationTime (..), Hashed, PlainText)
import Model.Token (Token (..))
import Model.User (User)

validateToken :: Token Hashed -> Token PlainText -> Bool
validateToken (Token hashed) (Token plainText) =
  let results =
        Argon2.checkPassword
          (Argon2.mkPassword plainText)
          (Argon2.PasswordHash hashed)
   in results == Argon2.PasswordCheckSuccess

getUser :: Token PlainText -> UTCTime -> [(User, ExpirationTime, Token Hashed)] -> Maybe User
getUser token currentTime users =
  (\(u, _, _) -> u)
    <$> find
      (\(_u, ExpirationTime expiry, hashed) -> validateToken hashed token && currentTime <= expiry)
      users