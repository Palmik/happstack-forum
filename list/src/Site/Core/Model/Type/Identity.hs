{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Site.Core.Model.Type.Identity
( Identity(..)
, IdentityID
, IdentityEntity
, newIdentity
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Data.IxSet     as IX
import qualified Data.SafeCopy  as SC
------------------------------------------------------------------------------
import qualified Web.Routes as WR
------------------------------------------------------------------------------
import           Site.Common.Model
import qualified Site.Core.Model.Type.Profile as IC
------------------------------------------------------------------------------

newIdentity :: Identity
newIdentity = Identity Nothing

type IdentityEntity = (IdentityID, Identity)

data Identity = Identity
    { identityProfile :: Maybe IC.Profile
    } deriving (Show, Data, Eq, Ord, Typeable)

newtype IdentityID = IdentityID Natural 
    deriving (Data, Eq, Ord, Typeable, Show, WR.PathInfo, SC.SafeCopy)

$(SC.deriveSafeCopy 0 'SC.base ''Identity)

instance AutoIncrementID IdentityID where
    nextID (IdentityID (Natural n)) = IdentityID $! Natural $! n + 1
    initID                          = IdentityID $! Natural 1

instance IX.Indexable (IdentityID, Identity) where
    empty = IX.ixSet
        [ IX.ixFun $ \ent -> [ fst ent ]
        ]


