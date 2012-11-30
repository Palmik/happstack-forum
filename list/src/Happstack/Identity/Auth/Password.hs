{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Happstack.Identity.Auth.Password
( I.HasPasswordManager(..)
, signin
, signup
, update
, delete

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
signin handle pass = do
    mres <- I.lookupPassword handle 
    case mres of
         Just hash
           | I.verifyPassword pass hash -> do 
               miid <- I.signinCredentials $ I.CredentialsPassword handle
               case miid of
                    Just iid -> handleSession iid >> return (Just iid)
                    Nothing  -> return Nothing
           | otherwise -> return Nothing 
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
signup handle pass = do
    bundle  <- I.makePasswordBundle handle pass 
    success <- I.insertPasswordBundle $! bundle 
    if success
       then do
         miid <- I.signupCredentials $ I.CredentialsPassword handle
         case miid of
             Just iid -> handleSession iid >> return (Just iid)
             Nothing  -> return Nothing
       else return Nothing

update :: ( Applicative m
          , MonadIO m
          , HA.FilterMonad HA.Response m
          , I.HasPasswordManager m
          , Show (I.IdentityID m)
          )
       => I.PasswordHandle 
       -> I.Password
       -> I.Password
       -> m Bool
update handle old new = do
    mres <- I.lookupPassword handle
    case mres of
        Just hash
          | I.verifyPassword old hash -> do
              bundle <- I.makePasswordBundle handle new
              I.updatePasswordBundle bundle 
          | otherwise -> return False
        Nothing -> return False

delete :: ( Applicative m
          , MonadIO m
          , HA.FilterMonad HA.Response m
          , I.HasPasswordManager m
          , Show (I.IdentityID m)
          )
       => I.PasswordHandle 
       -> I.Password
       -> m Bool
delete handle pass = do
    mres <- I.lookupPassword handle
    case mres of
        Just hash
          | I.verifyPassword pass hash -> I.deletePasswordBundle handle 
          | otherwise -> return False
        Nothing -> return False

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

