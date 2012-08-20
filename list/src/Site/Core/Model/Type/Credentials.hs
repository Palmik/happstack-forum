{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Site.Core.Model.Type.Credentials
( Credentials(..)
, CredentialsID
, CredentialsEntity
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Identity as HA (Credentials(..))
------------------------------------------------------------------------------
import qualified Data.IxSet as IxSet
import qualified Data.SafeCopy as SC
------------------------------------------------------------------------------
import           Site.Common.Model
import qualified Site.Core.Model.Type.Identity as IC
------------------------------------------------------------------------------

type CredentialsEntity = (CredentialsID, Credentials)

data Credentials = Credentials
    { credentialsIdentityID :: IC.IdentityID
    , credentialsData       :: HA.Credentials
    } deriving (Data, Eq, Ord, Typeable, Show)

newtype CredentialsID = CredentialsID Int64
     deriving (Data, Eq, Ord, Typeable, Show, SC.SafeCopy)

instance AutoIncrementID CredentialsID where
    nextID (CredentialsID n) = CredentialsID $! n + 1
    initID                   = CredentialsID 1

$(SC.deriveSafeCopy 0 'SC.base ''Credentials)

instance IxSet.Indexable (CredentialsID, Credentials) where
    empty = IxSet.ixSet
        [ IxSet.ixFun $ \ent -> [ fst ent ]
        , IxSet.ixFun $ \ent -> [ credentialsIdentityID $ snd ent ]
        , IxSet.ixFun $ \ent -> [ credentialsData $ snd ent ]
        ]
