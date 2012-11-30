{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Site.Forum.Model.Type.Post
( Post(..)
, PostData(..)
, PostDataExtra(..)
, PostID
, PostEntity
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Data.IxSet    as IX
import qualified Data.SafeCopy as SC
import qualified Data.Text     as TS
------------------------------------------------------------------------------
import qualified Web.Routes as WR
------------------------------------------------------------------------------
import           Site.Common.Model
------------------------------------------------------------------------------
import qualified Site.Core.Model.Type.Identity as IC
import qualified Site.Forum.Model.Type.Forum   as IF
------------------------------------------------------------------------------

type PostEntity = (PostID, Post)

data Post = Post
    { postData :: PostData
    , postDataExtra :: PostDataExtra
    } deriving (Data, Eq, Ord, Typeable, Show)

data PostData = PostData
    { postAuthor :: Maybe IC.IdentityID
    , postTitle :: TS.Text
    , postForums :: [IF.ForumID]
    , postContent :: Content 
    } deriving (Data, Eq, Ord, Typeable, Show)

data PostDataExtra = PostDataExtra
    { postCreated :: UTCTime
    } deriving (Data, Eq, Ord, Typeable, Show)

newtype PostID = PostID Natural 
    deriving (Data, Eq, Ord, Typeable, Show, SC.SafeCopy, WR.PathInfo, AutoIncrementID)

instance IX.Indexable (PostID, Post) where
    empty = IX.ixSet
      [ IX.ixFun $ \e -> [ key e ] -- ^ The Post's primary ID.
      , IX.ixFun $ \e -> postForums . postData $ value e -- ^ The Forum IDs where the Post is displayed.
      , IX.ixFun $ \e -> [ postTitle . postData $ value e ] -- ^ The Post's title.
      ] 

$(SC.deriveSafeCopy 0 'SC.base ''PostData)
$(SC.deriveSafeCopy 0 'SC.base ''PostDataExtra)
$(SC.deriveSafeCopy 0 'SC.base ''Post)


