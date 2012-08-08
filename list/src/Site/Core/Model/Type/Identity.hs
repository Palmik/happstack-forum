{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Site.Core.Model.Type.Identity
( Identity(..)
, IdentityID
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Identity as HA (Credentials)
------------------------------------------------------------------------------
import           Data.IxSet             (IxSet)
import qualified Data.IxSet    as IxSet
import qualified Data.SafeCopy as SC
------------------------------------------------------------------------------
import           Site.Common.Model
import qualified Site.Core.Model.Type.Profile as IC
------------------------------------------------------------------------------

data Identity = Identity
    { identityProfile :: Maybe IC.Profile
    } deriving (Eq, Ord, Typeable)
$(SC.deriveSafeCopy 0 'SC.base ''Identity)

newtype IdentityID = IdentityID Int64 deriving (Eq, Ord, Typeable)
$(SC.deriveSafeCopy 0 'SC.base ''IdentityID)

instance AutoIncrementID IdentityID where
    nextID (IdentityID n) = IdentityID $! n + 1
    initID                = IdentityID 1

instance IxSet.Indexable (IdentityID, Identity) where
    empty = IxSet.ixSet
        [ IxSet.ixFun $ \ent -> [ fst ent ]
        , IxSet.ixFun $ \ent -> [ snd ent ]
        ]


