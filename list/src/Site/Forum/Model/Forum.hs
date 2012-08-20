{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}

module Site.Forum.Model.Forum
( insert
, lookupByID
, lookupByPath
, getPageByCreation
, getListPageByCreation
, isUnique
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
import qualified Site.Forum.Model.Type       as IF
import qualified Site.Forum.Model.ACID.Forum as IF.Forum
------------------------------------------------------------------------------

insert :: (Functor m, MonadIO m, HA.HasAcidState m IF.ForumState) 
       => IF.Forum
       -> m (Maybe IF.ForumID)
insert = HA.update . IF.Forum.Insert
{-# INLINE insert #-}

lookupByID :: (Functor m, MonadIO m, HA.HasAcidState m IF.ForumState) 
           => IF.ForumID
           -> m (Maybe IF.Forum)
lookupByID = HA.query . IF.Forum.LookupByID
{-# INLINE lookupByID #-}

lookupByPath :: (Functor m, MonadIO m, HA.HasAcidState m IF.ForumState)
             => Path
             -> m (Maybe IF.ForumEntity)
lookupByPath = HA.query . IF.Forum.LookupByPath
{-# INLINE lookupByPath #-} 

getPageByCreation :: (Functor m, MonadIO m, HA.HasAcidState m IF.ForumState)
                  => IF.ForumID    
                  -> Page
                  -> Order
                  -> m [IF.PostEntity]
getPageByCreation fid p = HA.query . IF.Forum.GetPageByCreation fid p
{-# INLINE getPageByCreation #-}

getListPageByCreation :: (Functor m, MonadIO m, HA.HasAcidState m IF.ForumState)
                      => Page
                      -> Order
                      -> m [IF.ForumEntity]
getListPageByCreation p = HA.query . IF.Forum.GetListPageByCreation p
{-# INLINE getListPageByCreation #-}

isUnique :: (Functor m, MonadIO m, HA.HasAcidState m IF.ForumState)
         => IF.Forum
         -> m Bool
isUnique = HA.query . IF.Forum.IsUnique
{-# INLINE isUnique #-}

