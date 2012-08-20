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
import qualified Site.Forum.Model.Type      as IF
import qualified Site.Forum.Model.ACID.Post as IF.Post
------------------------------------------------------------------------------

insert :: (Functor m, MonadIO m, HA.HasAcidState m IF.ForumState)
       => IF.Post
       -> m IF.PostID
insert = HA.update . IF.Post.Insert 
{-# INLINE insert #-}

lookupByID :: (Functor m, MonadIO m, HA.HasAcidState m IF.ForumState)
           => IF.PostID
           -> m (Maybe IF.Post)
lookupByID = HA.query . IF.Post.LookupByID
{-# INLINE lookupByID #-}

