module Happstack.Identity.Auth.Session
( Session(..)
, SessionKey(..)
, SessionCookie(..)
) where

------------------------------------------------------------------------------
import qualified Happstack.Server as HA
------------------------------------------------------------------------------
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS (pack)
------------------------------------------------------------------------------
import qualified Crypto.PasswordStore as PS
------------------------------------------------------------------------------
import qualified Happstack.Identity.Types as I
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | SESSION

newtype SessionKey = SessionKey { unSessionKey :: BS.ByteString }
data Session m = Session
    { sessionIdentityID :: IdentityID m
  --  sessionExpiration :: UTCTime
  --  ...
    }

makeSession :: (MonadIO m, FilterMonad Response m, I.HasAuthManager m)
               => I.IdentityID m
               -> m (Session, SessionKey, SessionCookie)
makeSession iid = do
    skey <- makeSessionKey iid
    let session = Session iid
    let scookie = SessionCookie skey
    return (session, skey, scookie)

------------------------------------------------------------------------------
-- | COOKIE

    newtype SessionCookie = SessionCookie { unSessionCookie :: SessionKey }

sessionCookieKey :: String
sessionCookieKey = "auth_cookie"

addSessionCookie :: (MonadIO m, FilterMonad Response m)
                 -> SessionCookie
                 -> m ()
addSessionCookie mlife (SessionCookie iids) = HA.addCookie life cookie
    where life   = HA.Session
          cookie = HA.Cookie "1" "/" "" sessionCookieKey iids True True

expireSessionCookie :: (MonadIO m, FilterMonad Response m)
                    => m ()
expireSessionCookie = HA.expireCookie sessionCookieKey

------------------------------------------------------------------------------
-- | UTILITY

makeSessionKey :: (MonadIO m, FilterMonad Response m, I.HasAuthManager m)
               => I.IdentityID m
               -> m SessionKey
makeSessionKey iid = do
    rand <- liftIO $ exportSalt <$> genSaltIO
    return $! SessionKey $! BS.pack (I.encodeIdentityID iid) ++ rand


    