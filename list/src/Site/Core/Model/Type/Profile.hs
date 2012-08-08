{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Site.Core.Model.Type.Profile
( Profile(..)
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Data.SafeCopy as SC
------------------------------------------------------------------------------
import           Site.Common.Model
------------------------------------------------------------------------------

data Profile = Profile
    { profileName :: Text
    } deriving (Eq, Ord, Typeable)

$(SC.deriveSafeCopy 0 'SC.base ''Profile)


