{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Site.Forum.Model.ACID.Forum
( insert
, Insert(..)
, insertRaw
, InsertRaw(..)
, lookupByID
, LookupByID(..)
, lookupByPath
, LookupByPath(..)
, getPageByCreation
, GetPageByCreation(..)
, getListPageByCreation
, GetListPageByCreation(..)
, isUnique
, IsUnique(..)

, ForumDataIO(..)


, events
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import           Control.Monad.State
import           Control.Monad.Reader
------------------------------------------------------------------------------
import qualified Data.SafeCopy as SC
import qualified Data.Acid     as AS
import qualified Data.IxSet    as IX  
import           Data.IxSet          ((@=))
------------------------------------------------------------------------------
import           Site.Common.Model
import qualified Site.Forum.Model.Type  as IF
------------------------------------------------------------------------------

data ForumDataIO = ForumDataIO
    { ioforumCreated :: UTCTime
    } deriving (Data, Eq, Ord, Typeable)
$(SC.deriveSafeCopy 0 'SC.base ''ForumDataIO) 

makeComplete :: ForumDataIO
             -> IF.ForumData
             -> AS.Query IF.ForumState (Maybe IF.Forum)
makeComplete ForumDataIO{..} fdata@IF.ForumData{..} =
    -- ^ We check whether this form is 'top-level'
    case forumParent of
        -- ^ It has a parent.
        Just par -> do
          -- ^ But does it actually exist?
          mres <- fmap IF.forumDataExtra <$> lookupByID par
          case mres of
              Just pextra -> return $! Just $! fromParentExtra pextra
              Nothing     -> return Nothing
        -- ^ It does not have a parent.
        Nothing -> return $! Just IF.Forum
          { IF.forumData = fdata
          , IF.forumDataExtra = IF.ForumDataExtra
              { IF.forumCreated = ioforumCreated    
              , IF.forumPath = Path [forumPathSegment]
              , IF.forumAncestors = []
              }
          }
    where
      -- | In this function, we have got the extra data of the parent
      -- forum.
      fromParentExtra IF.ForumDataExtra{..} = IF.Forum
        { IF.forumData = fdata
        , IF.forumDataExtra = IF.ForumDataExtra
            { IF.forumCreated = ioforumCreated
            , IF.forumPath = Path $ forumPathSegment : unPath forumPath
            , IF.forumAncestors = fromJust forumParent : forumAncestors -- ^ We can safely fromJust here, since we know the parent exists.
            }
        }

-- | Returns False if forum with such parent and path segment exists,
-- otherwise returns True.
isUnique :: IF.ForumData
         -> AS.Query IF.ForumState Bool
isUnique IF.ForumData{..} = do
    db <- asks IF.stForumDB
    return $ isNothing $ IX.getOne ((db @= Parent forumParent) @= forumPathSegment)
{-# INLINE isUnique #-}

-- | Given the IO data (that we can not generate ourselves here) and given
-- the forum data, we check all neccessary things (uniqueness, etc.) and
-- if all goes well, we generated the whole forum and insert it.
insert :: ForumDataIO
       -> IF.ForumData
       -> AS.Update IF.ForumState (Maybe (IF.ForumID, IF.ForumDataExtra)) -- ^ Returns Nothing in case the parent did not exist.
insert fdataio fdata = do
    -- ^ Would it be unique?
    unique <- AS.runQuery $ isUnique fdata
    if unique
       then do
          mforum <- AS.runQuery $ makeComplete fdataio fdata 
          case mforum of
              Just forum@(IF.Forum _ extra) -> Just . (\fid -> (fid, extra)) <$> insertRaw forum 
              Nothing -> return Nothing
      else return Nothing

-- | Given a forum, we just insert it -- no checks nor changes to the forum
-- are performed.
insertRaw :: IF.Forum
          -> AS.Update IF.ForumState IF.ForumID
insertRaw forum = do
    st@IF.ForumState{..} <- get
    put st { IF.stNextForumID = nextID stNextForumID
           , IF.stForumDB = IX.insert (stNextForumID, forum) stForumDB
           }
    return stNextForumID
{-# INLINE insertRaw #-}

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


$(AS.makeEvents "events" ''IF.ForumState [ 'insert
                                         , 'insertRaw
                                         , 'lookupByID
                                         , 'lookupByPath
                                         , 'getPageByCreation
                                         , 'getListPageByCreation
                                         , 'isUnique
                                         ])


