{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Site.Core.Model.State
( HasCore
, CoreState(..)
, initCoreState
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.State
import           Control.Monad.RWS
------------------------------------------------------------------------------
import           Data.IxSet                  (IxSet)
import qualified Data.IxSet         as IxSet
import qualified Data.SafeCopy      as SC
------------------------------------------------------------------------------
import qualified Happstack.Server                 as HA
import qualified Happstack.State                  as HA
import qualified Happstack.Identity               as HA
------------------------------------------------------------------------------
import qualified Web.Routes           as WR
import qualified Web.Routes.Happstack as WR ()
------------------------------------------------------------------------------
import           Site.Common.Model
import qualified Site.Core.Model.Type.Identity    as IC
import qualified Site.Core.Model.Type.Credentials as IC
------------------------------------------------------------------------------

class ( Show (HA.IdentityID m), HA.IdentityID m ~ IC.IdentityID
      , HA.Happstack m, HA.HasPasswordManager m
      , HA.HasAcidState m CoreState
      ) => HasCore m where

instance (HasCore m)           => HasCore (StateT s m)
instance (HasCore m)           => HasCore (ReaderT r     m)
instance (HasCore m, Monoid w) => HasCore (WriterT   w   m)
instance (HasCore m, Monoid w) => HasCore (RWST    r w s m)
instance (HasCore m)           => HasCore (WR.RouteT url m)

data CoreState = CoreState
    { stNextCredentialsID :: IC.CredentialsID
    , stNextIdentityID    :: IC.IdentityID
    , stCredentialsDB :: IxSet IC.CredentialsEntity
    , stIdentityDB    :: IxSet IC.IdentityEntity
    } deriving (Data, Eq, Ord, Typeable, Show)

$(SC.deriveSafeCopy 0 'SC.base ''CoreState)

initCoreState :: CoreState
initCoreState = CoreState
    { stNextCredentialsID = initID
    , stNextIdentityID    = initID
    , stCredentialsDB = IxSet.empty
    , stIdentityDB    = IxSet.empty
    }
