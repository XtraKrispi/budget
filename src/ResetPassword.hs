module ResetPassword where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Random (MonadRandom (getRandomRs))
import Data.Foldable (find)
import Data.Password.Argon2 qualified as Argon2
import Data.Text (pack)
import Data.Time (UTCTime)
import Model (Hashed, PlainText, Token (..), User)

generateToken :: (MonadIO m, MonadRandom m) => m (Token PlainText, Token Hashed)
generateToken = do
  str <- pack . take 64 <$> getRandomRs ('=', 'z')
  hashed <- Token . Argon2.unPasswordHash <$> Argon2.hashPassword (Argon2.mkPassword str)
  pure (Token str, hashed)

validateToken :: Token Hashed -> Token PlainText -> Bool
validateToken (Token hashed) (Token plainText) =
  let results =
        Argon2.checkPassword
          (Argon2.mkPassword plainText)
          (Argon2.PasswordHash hashed)
   in results == Argon2.PasswordCheckSuccess

getUser :: Token PlainText -> UTCTime -> [(User, UTCTime, Token Hashed)] -> Maybe User
getUser token currentTime users =
  (\(u, _, _) -> u)
    <$> find
      (\(_u, expiry, hashed) -> validateToken hashed token && currentTime <= expiry)
      users