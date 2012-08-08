{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Site.Common.Model
( AutoIncrementID(..)
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------ 

-- | To make your type and `Entity`, make it instance of this type-class.
class AutoIncrementID a where
    nextID :: a -> a
    initID :: a