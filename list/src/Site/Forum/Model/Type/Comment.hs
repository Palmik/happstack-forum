{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Site.Forum.Model.Type.Comment
( Comment(..)
, CommentData(..)
, CommentDataExtra(..)
, CommentID
, CommentEntity
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Data.IxSet     as IX
import qualified Data.SafeCopy  as SC
------------------------------------------------------------------------------
import qualified Web.Routes as WR
------------------------------------------------------------------------------
import           Site.Common.Model
------------------------------------------------------------------------------
import qualified Site.Core.Model.Type as IC
import qualified Site.Forum.Model.Type.Post as IF
------------------------------------------------------------------------------

type CommentEntity = (CommentID, Comment)

data Comment = Comment
    { commentData :: CommentData
    , commentDataExtra :: CommentDataExtra
    } deriving (Data, Eq, Ord, Typeable)

data CommentData = CommentData
    { commentAuthor :: Maybe IC.IdentityID
    , commentContent :: Content 
    , commentParent :: Maybe CommentID
    , commentPost :: IF.PostID
    } deriving (Data, Eq, Ord, Typeable, Show)

data CommentDataExtra = CommentDataExtra
    { commentAncestors :: [CommentID]
    , commentCreated :: UTCTime
    } deriving (Data, Eq, Ord, Typeable, Show)

newtype CommentID = CommentID Natural
    deriving (Data, Eq, Ord, Typeable, Show, SC.SafeCopy, WR.PathInfo, AutoIncrementID)


instance IX.Indexable (CommentID, Comment) where
    empty = IX.ixSet
      [ IX.ixFun $ \e -> [ key e ]
      , IX.ixFun $ \e -> [ Parent . commentParent . commentData $ value e ]
      , IX.ixFun $ \e -> map Ancestor . commentAncestors . commentDataExtra $ value e
      , IX.ixFun $ \e -> [ commentPost . commentData $ value e ]
      ]

$(SC.deriveSafeCopy 0 'SC.base ''CommentData)
$(SC.deriveSafeCopy 0 'SC.base ''CommentDataExtra)
$(SC.deriveSafeCopy 0 'SC.base ''Comment)

