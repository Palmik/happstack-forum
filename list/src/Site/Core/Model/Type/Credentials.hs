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
import qualified Web.Routes as WR
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

newtype CredentialsID = CredentialsID Natural 
     deriving (Data, Eq, Ord, Typeable, Show, AutoIncrementID, SC.SafeCopy, WR.PathInfo)

$(SC.deriveSafeCopy 0 'SC.base ''Credentials)

instance IxSet.Indexable (CredentialsID, Credentials) where
    empty = IxSet.ixSet
        [ IxSet.ixFun $ \ent -> [ key ent ]
        , IxSet.ixFun $ \ent -> [ credentialsIdentityID $ value ent ]
        , IxSet.ixFun $ \ent -> [ credentialsData $ value ent ]
        ]
