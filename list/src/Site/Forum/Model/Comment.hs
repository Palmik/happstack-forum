{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}

module Site.Forum.Model.Comment
( insert
, getThreadByCreation
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import           Control.Monad.Trans(MonadIO(..))
------------------------------------------------------------------------------
import qualified Happstack.State as HA
------------------------------------------------------------------------------
import qualified Site.Forum.Model.Type         as IF
import qualified Site.Forum.Model.ACID.Comment as IF.Comment
------------------------------------------------------------------------------

insert :: (Functor m, MonadIO m, HA.HasAcidState m IF.ForumState)
       => IF.Comment
       -> m IF.CommentID
insert = HA.update . IF.Comment.Insert 
{-# INLINE insert #-}

getThreadByCreation :: (Functor m, MonadIO m, HA.HasAcidState m IF.ForumState)
                    => IF.PostID
                    -> m [IF.CommentEntity]
getThreadByCreation = HA.query . IF.Comment.GetThreadByCreation
{-# INLINE getThreadByCreation #-}

