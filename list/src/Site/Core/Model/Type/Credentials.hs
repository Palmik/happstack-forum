{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Site.Core.Model.Type.Credentials
( HA.Credentials(..)
, CredentialsID
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Identity as HA (Credentials(..))
------------------------------------------------------------------------------
import           Data.IxSet          (IxSet)
import qualified Data.IxSet as IxSet
import qualified Data.SafeCopy as SC
------------------------------------------------------------------------------
import           Site.Common.Model
import qualified Site.Core.Model.Type.Profile as IC
------------------------------------------------------------------------------

newtype CredentialsID = CredentialsID Int64 deriving (Eq, Ord, Typeable)
$(SC.deriveSafeCopy 0 'SC.base ''CredentialsID)

instance AutoIncrementID CredentialsID where
    nextID (CredentialsID n) = CredentialsID $! n + 1
    initID                   = CredentialsID 1


instance IxSet.Indexable (CredentialsID, HA.Credentials) where
    empty = IxSet.ixSet
        [ IxSet.ixFun $ \ent -> [ fst ent ]
        , IxSet.ixFun $ \ent -> [ snd ent ]
        ]