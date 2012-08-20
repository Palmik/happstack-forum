{-# LANGUAGE TypeFamilies    #-}

module Site.Core.Model
( maybeIdentity
, HA.maybeIdentityID
, requireIdentity
, requireIdentityID

  -- * Common things
, module Export
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Identity as HA
import qualified Happstack.State    as HA
------------------------------------------------------------------------------
import qualified Data.Acid          as AS
import qualified Data.Acid.Advanced as AS
------------------------------------------------------------------------------
import qualified Site.Core.Model.State            as IC
import qualified Site.Core.Model.Type             as IC
import qualified Site.Core.Model.ACID.Identity    as IC.Identity
import qualified Site.Core.Model.ACID.Profile     as IC.Profile
import qualified Site.Core.Model.ACID.Credentials as IC.Credentials
------------------------------------------------------------------------------
import           Site.Core.Model.Type as Export
------------------------------------------------------------------------------

instance AS.IsAcidic IC.CoreState where
    acidEvents = IC.Identity.events <>
                 IC.Profile.events  <>
                 IC.Credentials.events

------------------------------------------------------------------------------
-- | CONVENIENCE FUNCTIONS

maybeIdentity :: IC.HasCore m
               => m (Maybe IC.IdentityEntity)
maybeIdentity = do
    miid <- HA.maybeIdentityID
    case miid of
        Just iid -> do
          res <- HA.query $ IC.Identity.LookupByID iid
          return $ maybe Nothing (\i -> Just (iid, i)) res
        Nothing  -> return Nothing

requireIdentityID :: IC.HasCore m
                  => m IC.IdentityID
requireIdentityID = do
    miid <- HA.maybeIdentityID
    case miid of
        Just iid -> return iid
        Nothing  -> mzero

requireIdentity :: IC.HasCore m
                => m IC.IdentityEntity
requireIdentity = do
    midentity <- maybeIdentity
    case midentity of
        Just identity -> return identity
        Nothing       -> mzero

