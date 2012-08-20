{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Site.Forum.Model.Type.Forum
( Forum(..)
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
import           Site.Common.Model
------------------------------------------------------------------------------

type ForumEntity = (ForumID, Forum)

data Forum = Forum
    { forumParent :: Maybe ForumID
    , forumAncestors :: [ForumID] 
    , forumPathSegment :: PathSegment
    , forumPath :: Path
    , forumName :: TS.Text
    , forumCreated :: UTCTime
    } deriving (Data, Eq, Ord, Typeable, Show)

newtype ForumID = ForumID Int64
      deriving (Data, Eq, Ord, Typeable, Show, SC.SafeCopy)

instance AutoIncrementID ForumID where
    nextID (ForumID n) = ForumID $! n +1
    initID             = ForumID 1

instance IX.Indexable (ForumID, Forum) where
    empty = IX.ixSet
      [ IX.ixFun $ \e -> [ fst e ] -- ^ The Forum's primary key (aka ForumID)
      , IX.ixFun $ \e -> [ forumParent $ snd e ] -- ^ The Forum's direct ancestor (aka Parent)
      , IX.ixFun $ \e -> [ map Ancestor $ forumAncestors $ snd e ] -- ^ The Forum's ancestors (including Parent)
      , IX.ixFun $ \e -> [ forumPathSegment $ snd e ] -- ^ The Forum's path segment
      , IX.ixFun $ \e -> [ forumPath $ snd e ] -- ^ The full Forum's path (concatenated path piece with ancestors' path pieces)
      ] 

$(SC.deriveSafeCopy 0 'SC.base ''Forum)
