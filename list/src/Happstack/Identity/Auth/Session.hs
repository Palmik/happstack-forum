{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Happstack.Identity.Auth.Session
( I.Session(..)
, I.SessionKey(..)
, I.SessionCookie(..)

, createSessionData
, createSessionCookie
, deleteSessionCookie
, lookupSessionCookie
, hasSessionCookie
) where

------------------------------------------------------------------------------
import           Prelude
------------------------------------------------------------------------------
import qualified Happstack.Server as HA
------------------------------------------------------------------------------
import	         Control.Applicative
import       		 Control.Monad.Trans (MonadIO(..))
------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS (pack, unpack)
------------------------------------------------------------------------------
import qualified Crypto.PasswordStore as PS
------------------------------------------------------------------------------
import qualified Happstack.Identity.Types              as I
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | SESSION

createSessionData :: ( Applicative m, MonadIO m
                     , HA.FilterMonad HA.Response m
                     , I.HasIdentityManager m
                     )
                   => I.IdentityID m
                   -> m (I.Session (I.IdentityID m), I.SessionKey, I.SessionCookie)
createSessionData iid = do
    skey <- makeSessionKey
    let session = I.Session iid
    let scookie = I.SessionCookie skey
    return (session, skey, scookie)

------------------------------------------------------------------------------
-- | COOKIE

sessionCookieKey :: String
sessionCookieKey = "session_cookie"

createSessionCookie :: (MonadIO m, HA.FilterMonad HA.Response m)
                    => I.SessionCookie
                    -> m ()
createSessionCookie (I.SessionCookie (I.SessionKey iids)) =
    HA.addCookie life cookie
    where life   = HA.Session
          cookie = HA.Cookie "1" "/" "" sessionCookieKey (BS.unpack iids) False False 

deleteSessionCookie :: (MonadIO m, HA.FilterMonad HA.Response m)
                    => m ()
deleteSessionCookie = HA.expireCookie sessionCookieKey

lookupSessionCookie :: ( Applicative m, MonadIO m, HA.FilterMonad HA.Response m
                       , HA.HasRqData m, HA.ServerMonad m
                       )
                    => m (Maybe I.SessionCookie)
lookupSessionCookie = do
    mval <- fromEither <$> HA.getDataFn (HA.lookCookieValue sessionCookieKey)
    case mval of
        Just val -> return . Just . I.SessionCookie . I.SessionKey $ BS.pack val
        Nothing  -> return Nothing
    where
      fromEither (Left _)  = Nothing
      fromEither (Right x) = Just x

hasSessionCookie :: ( Applicative m, MonadIO m, HA.FilterMonad HA.Response m
                    , HA.HasRqData m, HA.ServerMonad m
                    )
                 => m Bool
hasSessionCookie = isRight <$> HA.getDataFn (HA.lookCookieValue sessionCookieKey)
    where
      isRight (Right _) = True
      isRight _         = False

------------------------------------------------------------------------------
-- | UTILITY

makeSessionKey :: (Applicative m, MonadIO m)
               => m I.SessionKey
makeSessionKey = I.SessionKey . PS.exportSalt <$> liftIO PS.genSaltIO


    
