{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts  #-}

module Site.Core.Model.Credentials
( signin
, signup
, lookupByID
, lookupByIdentityID
, delete 

) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import           Control.Monad.Reader 
------------------------------------------------------------------------------
import qualified Happstack.Identity               as HA
import qualified Happstack.Identity.Auth.Password as HA (HasPasswordManager(..))
import qualified Happstack.State                  as HA
------------------------------------------------------------------------------
import qualified Site.Core.Model.State            as IC
import qualified Site.Core.Model.Type             as IC
import qualified Site.Core.Model.ACID.Credentials as IC.Credentials
------------------------------------------------------------------------------

signin :: (Functor m, MonadIO m, HA.HasAcidState m IC.CoreState)
       => HA.Credentials
       -> m (Maybe IC.IdentityID)
signin = HA.query . IC.Credentials.Signin
{-# INLINE signin #-}

signup :: (Functor m, MonadIO m, HA.HasAcidState m IC.CoreState)
       => HA.Credentials
       -> m (Maybe IC.IdentityID)
signup = HA.update . IC.Credentials.Signup
{-# INLINE signup #-}

lookupByID :: (Functor m, MonadIO m, HA.HasAcidState m IC.CoreState)
           => IC.CredentialsID
           -> m (Maybe IC.Credentials)
lookupByID = HA.query . IC.Credentials.LookupByID
{-# INLINE lookupByID #-}

lookupByIdentityID :: (Functor m, MonadIO m, HA.HasAcidState m IC.CoreState)
                   => IC.IdentityID
                   -> m [(IC.CredentialsID, HA.Credentials)]
lookupByIdentityID = HA.query . IC.Credentials.LookupByIdentityID 
{-# INLINE lookupByIdentityID #-}

delete :: (Functor m, MonadIO m, HA.HasAcidState m IC.CoreState, HA.HasPasswordManager m)
       => IC.CredentialsID
       -> m Bool
delete cid = do
    mres <- lookupByID cid
    case mres of
        Just (IC.Credentials iid (HA.CredentialsPassword handle)) -> do
          success <- HA.deletePasswordBundle handle
          if success
             then HA.update (IC.Credentials.Delete cid) >> return True
             else return False
        Nothing -> return False

