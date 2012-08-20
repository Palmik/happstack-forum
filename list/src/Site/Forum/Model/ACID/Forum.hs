{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Site.Forum.Model.ACID.Forum
( insert
, lookupByID
, lookupByPath
, getPageByCreation

, Insert(..)
, LookupByID(..)
, LookupByPath(..)
, GetPageByCreation(..)
, GetListPageByCreation(..)
, IsUnique(..)

, events
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import           Control.Monad.State
import           Control.Monad.Reader
------------------------------------------------------------------------------
import qualified Data.Acid  as AS
import qualified Data.IxSet as IX  
import           Data.IxSet       ((@=))
------------------------------------------------------------------------------
import           Site.Common.Model
import qualified Site.Forum.Model.Type  as IF
------------------------------------------------------------------------------

insert :: IF.Forum -- ^ The forum itself (note that the forumPath field is ignored and is generated from forumPathPiece and forumParent)
       -> AS.Update IF.ForumState (Maybe IF.ForumID) -- ^ Returns Nothing in case the parent did not exist.
insert forum = do
    unique <- AS.runQuery $ isUnique forum 
    if unique
       then do
          mres <- AS.runQuery $ constructFromAncestors forum
          case mres of
              Just res -> do
                st@IF.ForumState{..} <- get
                put st { IF.stNextForumID = nextID stNextForumID
                       , IF.stForumDB = IX.insert (stNextForumID, res) stForumDB
                       }
                return $ Just stNextForumID
              Nothing  -> return Nothing
      else return Nothing

lookupByID :: IF.ForumID -> AS.Query IF.ForumState (Maybe IF.Forum)
lookupByID fid = do 
    db <- asks IF.stForumDB
    return $ value <$> IX.getOne (db @= fid)

lookupByPath :: Path -> AS.Query IF.ForumState (Maybe IF.ForumEntity)
lookupByPath path = do
    db <- asks IF.stForumDB
    return $ IX.getOne $ db @= path

getPageByCreation :: IF.ForumID -> Page -> Order -> AS.Query IF.ForumState [IF.PostEntity]
getPageByCreation fid p o = do 
    db <- asks IF.stPostDB
    return $ getPage (IX.Proxy :: IX.Proxy IF.PostID) p o $ db @= fid
     
getListPageByCreation :: Page -> Order -> AS.Query IF.ForumState [IF.ForumEntity]
getListPageByCreation p o =
    getPage (IX.Proxy :: IX.Proxy IF.ForumID) p o <$> asks IF.stForumDB

------------------------------------------------------------------------------
-- CONVENIENCE FUNCTIONS

-- | Given a Forum, this functions attempts to 'fix' its
-- forumAncestor and forumPath, if the forumParent des not exist, 
-- return Nothing.
constructFromAncestors :: IF.Forum -> AS.Query IF.ForumState (Maybe IF.Forum)
constructFromAncestors forum =
    case IF.forumParent forum of
        Just par -> do
          mres <- fmap (IF.forumAncestors &&& IF.forumPath) <$> lookupByID par
          case mres of
            Just (pa, pp) -> return $ Just $ consRes par pa pp
            Nothing       -> return Nothing 
        Nothing  -> return $ Just $ forum
          { IF.forumAncestors = []
          , IF.forumPath      = Path [ IF.forumPathSegment forum ]
          }
    where
      consRes par pa (Path pp) = forum
          { IF.forumAncestors = par : pa
          , IF.forumPath = Path $ IF.forumPathSegment forum : pp
          }

-- | Returns False if forum with such parent and path segment exists,
-- otherwise returns True.
isUnique :: IF.Forum
         -> AS.Query IF.ForumState Bool
isUnique forum = do
    db <- asks IF.stForumDB
    return $ isNothing $ IX.getOne ((db @= IF.forumParent forum) @= IF.forumPathSegment forum)
{-# INLINE isUnique #-}

$(AS.makeEvents "events" ''IF.ForumState [ 'insert
                                         , 'lookupByID
                                         , 'lookupByPath
                                         , 'getPageByCreation
                                         , 'getListPageByCreation
                                         , 'isUnique
                                         ])


