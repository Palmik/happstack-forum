{-# LANGUAGE TemplateHaskell #-}

module Site.Forum.State
( ForumState(..)
, initForumState
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Data.Acid     as AS
import           Data.IxSet             (IxSet)
import qualified Data.IxSet    as IxSet
import qualified Data.SafeCopy as SC
------------------------------------------------------------------------------

data ForumState = ForumState {} deriving (Typeable)

instance AS.IsAcidic ForumState where

$(SC.deriveSafeCopy 0 'SC.base ''ForumState)

initForumState :: ForumState
initForumState = ForumState