{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Site.Forum.Model.Type.Post
( Post(..)
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

data Post = Post
    { postAuthor :: Maybe IC.IdentityID
    , postTitle :: TS.Text
    , postForums :: [IF.ForumID]
    , postContent :: TS.Text
    , postCreated :: UTCTime
    } deriving (Data, Eq, Ord, Typeable, Show)
$(SC.deriveSafeCopy 0 'SC.base ''Post)

newtype PostID = PostID Natural 
    deriving (Data, Eq, Ord, Typeable, SC.SafeCopy, WR.PathInfo, AutoIncrementID)

type PostEntity = (PostID, Post)

instance IX.Indexable (PostID, Post) where
    empty = IX.ixSet
      [ IX.ixFun $ \e -> [ key e ] -- ^ The Post's primary ID.
      , IX.ixFun $ \e -> postForums $ value e -- ^ The Forum IDs where the Post is displayed.
      , IX.ixFun $ \e -> [ postTitle $ value e ] -- ^ The Post's title.
      ] 

