{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}

module Site.Forum.Model.Post
( insert
, lookupByID
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
import qualified Site.Forum.Model.Type      as IF
import qualified Site.Forum.Model.ACID.Post as IF.Post
------------------------------------------------------------------------------

insert :: (Functor m, MonadIO m, HA.HasAcidState m IF.ForumState)
       => IF.PostData
       -> m IF.PostID
insert pdata = do
    pdataio <- makeDataIO pdata
    HA.update $ IF.Post.Insert pdataio pdata
{-# INLINE insert #-}

lookupByID :: (Functor m, MonadIO m, HA.HasAcidState m IF.ForumState)
           => IF.PostID
           -> m (Maybe IF.Post)
lookupByID = HA.query . IF.Post.LookupByID
{-# INLINE lookupByID #-}

makeDataIO :: MonadIO m => IF.PostData -> m IF.Post.PostDataIO
makeDataIO _ = do
    time <- liftIO getCurrentTime
    return IF.Post.PostDataIO
      { IF.Post.iopostCreated = time
      }


