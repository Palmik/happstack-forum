{-# LANGUAGE FlexibleContexts #-}

module Site.Core.Model.Identity
( insert
, update
, upsert
, lookupByID
, lookupByHandle
, lookupIDByHandle
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import           Control.Monad.Trans(MonadIO(..))
------------------------------------------------------------------------------
import qualified Happstack.State as HA
------------------------------------------------------------------------------
import qualified Site.Core.Model.Type          as IC
import qualified Site.Core.Model.ACID.Identity as IC.Identity
------------------------------------------------------------------------------

insert :: (Functor m, MonadIO m, HA.HasAcidState m IC.CoreState)
       => IC.Identity
       -> m IC.IdentityID
insert i = HA.update $ IC.Identity.Insert i
{-# INLINE insert #-}
 
update :: (Functor m, MonadIO m, HA.HasAcidState m IC.CoreState)
       => IC.IdentityID
       -> IC.Identity
       -> m Bool
update iid i = HA.update $ IC.Identity.Update iid i
{-# INLINE update #-}

upsert :: (Functor m, MonadIO m, HA.HasAcidState m IC.CoreState)
       => IC.IdentityID
       -> IC.Identity
       -> m ()
upsert iid i = HA.update $ IC.Identity.Upsert iid i
{-# INLINE upsert #-}

lookupByID :: (Functor m, MonadIO m, HA.HasAcidState m IC.CoreState)
           => IC.IdentityID
           -> m (Maybe IC.Identity)
lookupByID iid = HA.query $ IC.Identity.LookupByID iid
{-# INLINE lookupByID #-}

lookupByHandle :: (Functor m, MonadIO m, HA.HasAcidState m IC.CoreState)
               => Text
               -> m (Maybe IC.IdentityEntity)
lookupByHandle handle = HA.query $ IC.Identity.LookupByHandle handle
{-# INLINE lookupByHandle #-}

lookupIDByHandle :: (Functor m, MonadIO m, HA.HasAcidState m IC.CoreState)
                 => Text
                 -> m (Maybe IC.IdentityID)
lookupIDByHandle handle = HA.query $ IC.Identity.LookupIDByHandle handle 
{-# INLINE lookupIDByHandle #-}






