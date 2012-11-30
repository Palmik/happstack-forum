{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Site.Forum.Model.Type.Forum
( Forum(..)
, ForumData(..)
, ForumDataExtra(..)
, ForumID
, ForumEntity
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Data.IxSet     as IX
import qualified Data.SafeCopy  as SC
import qualified Data.Text      as TS (Text)
------------------------------------------------------------------------------
import qualified Web.Routes as WR
------------------------------------------------------------------------------
import           Site.Common.Model
------------------------------------------------------------------------------

type ForumEntity = (ForumID, Forum)

data Forum = Forum
    { forumData :: ForumData
    , forumDataExtra :: ForumDataExtra
    } deriving (Data, Eq, Ord, Typeable, Show)

data ForumData = ForumData
    { forumParent :: Maybe ForumID
    , forumPathSegment :: PathSegment
    , forumTitle :: TS.Text
    , forumDescription :: Content
    } deriving (Data, Eq, Ord, Typeable, Show)

data ForumDataExtra = ForumDataExtra
    { forumCreated :: UTCTime
    , forumAncestors :: [ForumID]
    , forumPath :: Path
    } deriving (Data, Eq, Ord, Typeable, Show)

newtype ForumID = ForumID Natural 
    deriving (Data, Eq, Ord, Typeable, Show, SC.SafeCopy, WR.PathInfo, AutoIncrementID)

instance IX.Indexable (ForumID, Forum) where
    empty = IX.ixSet
      [ IX.ixFun $ \e -> [ fst e ]

      , IX.ixFun $ \e -> [ Parent . forumParent . forumData $ snd e ]
      , IX.ixFun $ \e -> [ forumPathSegment . forumData $ snd e ]
      
      , IX.ixFun $ \e -> [ map Ancestor . forumAncestors . forumDataExtra $ snd e ]
      , IX.ixFun $ \e -> [ forumPath . forumDataExtra $ snd e ]
      ] 

$(SC.deriveSafeCopy 0 'SC.base ''ForumData)
$(SC.deriveSafeCopy 0 'SC.base ''ForumDataExtra)
$(SC.deriveSafeCopy 0 'SC.base ''Forum)

