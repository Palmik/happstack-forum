{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Site.Core.Model.Type.Profile
( Profile(..)
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Data.SafeCopy as SC
------------------------------------------------------------------------------

data Profile = Profile
    { profileHandle :: Text
    } deriving (Show, Data, Eq, Ord, Typeable)

$(SC.deriveSafeCopy 0 'SC.base ''Profile)


