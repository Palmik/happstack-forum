{-# LANGUAGE TemplateHaskell #-}

module Site.Core.State
( CoreState(..)
, initCoreState
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Data.Acid     as AS
import           Data.IxSet             (IxSet)
import qualified Data.IxSet    as IxSet
import qualified Data.SafeCopy as SC
------------------------------------------------------------------------------
import           Site.Common.Model
import qualified Site.Core.Model.Type.Identity    as IC
import qualified Site.Core.Model.Type.Credentials as IC
------------------------------------------------------------------------------

data CoreState = CoreState
    { stateNextCredentialsID :: IC.CredentialsID
    , stateNextIdentityID    :: IC.IdentityID
    , stateCredentials :: IxSet (IC.CredentialsID, IC.Credentials)
    , stateIdentities  :: IxSet (IC.IdentityID, IC.Identity)
    } deriving (Typeable)

instance AS.IsAcidic CoreState where

$(SC.deriveSafeCopy 0 'SC.base ''CoreState)

initCoreState :: CoreState
initCoreState = CoreState
    { stateNextCredentialsID = initID
    , stateNextIdentityID    = initID
    , stateCredentials = IxSet.empty
    , stateIdentities  = IxSet.empty
    }