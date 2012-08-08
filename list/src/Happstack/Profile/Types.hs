module Happstack.Profile.Types
( HasProfile(..)
) where

class HasProfile m where
    type Profile   m :: *
    type ProfileID m :: *