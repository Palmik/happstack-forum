{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}

module Site.Forum.Model.Comment
( insert
, lookupByID
, getThreadByCreation
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import           Control.Monad.Trans(MonadIO(..))
------------------------------------------------------------------------------
import qualified Happstack.State as HA
------------------------------------------------------------------------------
import           Site.Common.Model
------------------------------------------------------------------------------
import qualified Site.Forum.Model.Type         as IF
import qualified Site.Forum.Model.ACID.Comment as IF.Comment
------------------------------------------------------------------------------

insert :: (Functor m, MonadIO m, HA.HasAcidState m IF.ForumState)
       => IF.CommentData
       -> m (Maybe IF.CommentID)
insert cdata = do
    cdataio <- makeDataIO cdata
    HA.update $ IF.Comment.Insert cdataio cdata
{-# INLINE insert #-}

{-
insertRaw :: (Functor m, MonadIO m, HA.HasAcidState m IF.ForumState)
          => IF.Comment
          -> m IF.CommentID
insertRaw = HA.update . IF.Comment.InsertRaw
{-# INLINE insertRaw #-}
-}

lookupByID :: (Functor m, MonadIO m, HA.HasAcidState m IF.ForumState)
           => IF.CommentID
           -> m (Maybe IF.Comment)
lookupByID = HA.query . IF.Comment.LookupByID
{-# INLINE lookupByID #-}

getThreadByCreation :: (Functor m, MonadIO m, HA.HasAcidState m IF.ForumState)
                    => IF.PostID
                    -> m [IF.CommentEntity]
getThreadByCreation = HA.query . IF.Comment.GetThreadByCreation
{-# INLINE getThreadByCreation #-}

makeDataIO :: MonadIO m => IF.CommentData -> m IF.Comment.CommentDataIO
makeDataIO _ = do
    time <- liftIO getCurrentTime
    return IF.Comment.CommentDataIO
      { IF.Comment.iocommentCreated = time
      }
{-# INLINE makeDataIO #-}

