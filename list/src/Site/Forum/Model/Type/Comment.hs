{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Site.Forum.Model.Type.Comment
( Comment(..)
, CommentID
, CommentEntity
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
import qualified Site.Core.Model.Type as IC
import qualified Site.Forum.Model.Type.Post as IF
------------------------------------------------------------------------------

data Comment = Comment
    { commentAuthor    :: Maybe IC.IdentityID
    , commentContent   :: TS.Text
    , commentParent    :: Maybe CommentID
    , commentAncestors :: [CommentID]
    , commentPost      :: IF.PostID
    , commentCreated   :: UTCTime
    } deriving (Data, Eq, Ord, Typeable)

newtype CommentID = CommentID Natural
    deriving (Data, Eq, Ord, Typeable, SC.SafeCopy, WR.PathInfo, AutoIncrementID)

$(SC.deriveSafeCopy 0 'SC.base ''Comment)

type CommentEntity = (CommentID, Comment)

instance IX.Indexable (CommentID, Comment) where
    empty = IX.ixSet
      [ IX.ixFun $ \e -> [ key e ]
      , IX.ixFun $ \e -> [ Parent . commentParent $ value e ]
      , IX.ixFun $ \e -> map Ancestor . commentAncestors $ value e
      , IX.ixFun $ \e -> [ commentPost $ value e ]
      ]

