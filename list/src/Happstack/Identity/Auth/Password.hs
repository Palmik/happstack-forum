{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Happstack.Identity.Auth.Password
( I.HasPasswordManager(..)
, signin
, signup

, module Export
) where
    
------------------------------------------------------------------------------
import           Prelude
------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans  (MonadIO(..))
------------------------------------------------------------------------------
import qualified Happstack.Server as HA
------------------------------------------------------------------------------
import qualified Happstack.Identity.Types               as I
import qualified Happstack.Identity.Auth.Password.Types as I
import qualified Happstack.Identity.Auth.Session        as I
------------------------------------------------------------------------------
import           Happstack.Identity.Auth.Password.Types as Export
------------------------------------------------------------------------------

signin :: ( Applicative m, MonadIO m
          , HA.FilterMonad HA.Response m
          , I.HasPasswordManager m
          )
       => I.PasswordHandle
       -> I.Password
       -> m (Maybe (I.IdentityID m))
signin name pass = do
    mres <- I.lookupPassword name 
    case mres of
         Just (pid, passhash) -> 
            if I.verifyPassword pass passhash
               then do
                 miid <- I.signinCredentials $ I.CredentialsPassword pid
                 case miid of
                      Just iid -> handleSession iid >> return (Just iid)
                      Nothing  -> return Nothing
               else return Nothing 
         Nothing  -> return Nothing

signup :: ( Applicative m
          , MonadIO m
          , HA.FilterMonad HA.Response m
          , I.HasPasswordManager m
          , Show (I.IdentityID m)
          )
       => I.PasswordHandle 
       -> I.Password
       -> m (Maybe (I.IdentityID m))
signup name pass = do
    bundle <- makePasswordBundle name pass 
    mpid   <- I.insertPasswordBundle $! bundle 
    case mpid of
         Just pid -> do
           miid <- I.signupCredentials $ I.CredentialsPassword pid
           case miid of
                Just iid -> handleSession iid >> return (Just iid)
                Nothing  -> return Nothing
         Nothing  -> return Nothing

------------------------------------------------------------------------------
-- | CONVENIENCE FUNCTIONS

handleSession :: ( Applicative m, MonadIO m
                 , HA.FilterMonad HA.Response m
                 , I.HasPasswordManager m
                 )
              => I.IdentityID m
              -> m ()
handleSession iid = do
    (s, skey, scookie) <- I.createSessionData iid
    I.createSessionCookie scookie 
    I.insertSession skey s   

makePasswordBundle :: (Applicative m, MonadIO m)
		               => I.PasswordHandle 
		               -> I.Password 
                   -> m I.PasswordBundle 
makePasswordBundle name pass = I.PasswordBundle name <$>
                                   I.makePasswordSaltedHash pass 13

