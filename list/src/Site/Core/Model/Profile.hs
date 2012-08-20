{-# LANGUAGE FlexibleContexts #-}

module Site.Core.Model.Profile
( insert
, update
, upsert
, delete
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import           Control.Monad.Trans(MonadIO(..))
------------------------------------------------------------------------------
import qualified Happstack.State as HA
------------------------------------------------------------------------------
import qualified Site.Core.Model.Type         as IC
import qualified Site.Core.Model.ACID.Profile as IC.Profile
------------------------------------------------------------------------------

insert :: (Functor m, MonadIO m, HA.HasAcidState m IC.CoreState)
       => IC.IdentityID
       -> IC.Profile
       -> m ()
insert iid p = HA.update $! IC.Profile.Insert iid p
{-# INLINE insert #-}

update :: (Functor m, MonadIO m, HA.HasAcidState m IC.CoreState)
       => IC.IdentityID
       -> IC.Profile
       -> m Bool
update iid p = HA.update $! IC.Profile.Update iid p
{-# INLINE update #-}

upsert :: (Functor m, MonadIO m, HA.HasAcidState m IC.CoreState)
       => IC.IdentityID
       -> IC.Profile
       -> m ()
upsert iid p = HA.update $! IC.Profile.Upsert iid p
{-# INLINE upsert #-}

delete :: (Functor m, MonadIO m, HA.HasAcidState m IC.CoreState)
       => IC.IdentityID
       -> m ()
delete iid = HA.update $! IC.Profile.Delete iid
{-# INLINE delete #-}




