{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Site.Forum.Model.ACID.Comment
( insert
, Insert(..)
, insertRaw
, InsertRaw(..)
, lookupByID
, LookupByID(..)
, getThreadByCreation
, GetThreadByCreation(..)

, CommentDataIO(..)

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
import qualified Site.Forum.Model.State as IF
import qualified Site.Forum.Model.Type  as IF
------------------------------------------------------------------------------

data CommentDataIO = CommentDataIO 
    { iocommentCreated :: UTCTime
    } deriving (Data, Eq, Ord, Typeable, Show)
$(SC.deriveSafeCopy 0 'SC.base ''CommentDataIO)

makeComplete :: CommentDataIO -> IF.CommentData -> AS.Query IF.ForumState (Maybe IF.Comment)
makeComplete CommentDataIO{..} cdata@IF.CommentData{..} =
    -- ^ We check whether the comment is 'top-level' 
    case commentParent of
        -- ^ It has a parent.
        Just par -> do
          -- ^ But does it actually exist?
          mres <- fmap IF.commentDataExtra <$> lookupByID par
          case mres of
              Just pextra -> return $! Just $! fromParentExtra pextra
              Nothing     -> return Nothing
        -- ^ It does not have a parent.
        Nothing -> return $! Just IF.Comment
          { IF.commentData = cdata
          , IF.commentDataExtra = IF.CommentDataExtra
              { IF.commentCreated = iocommentCreated
              , IF.commentAncestors = []
              }
          }
    where
      -- | In this function we construct the whole comment knowing the
      -- extra data of it's parent and the comment's data.
      fromParentExtra IF.CommentDataExtra{..} = IF.Comment
        { IF.commentData = cdata
        , IF.commentDataExtra = IF.CommentDataExtra
            { IF.commentCreated = iocommentCreated
            , IF.commentAncestors = fromJust commentParent : commentAncestors
            }
        }

insert :: CommentDataIO
       -> IF.CommentData
       -> AS.Update IF.ForumState (Maybe IF.CommentID)
insert cdataio cdata = do
    mcomment <- AS.runQuery $ makeComplete cdataio cdata
    case mcomment of
        Just comment -> Just <$> insertRaw comment
        Nothing      -> return Nothing

insertRaw :: IF.Comment
          -> AS.Update IF.ForumState IF.CommentID
insertRaw comment = do
    st@IF.ForumState{..} <- get
    put st { IF.stNextCommentID = nextID stNextCommentID
           , IF.stCommentDB = IX.insert (stNextCommentID, comment) stCommentDB
           }
    return stNextCommentID
{-# INLINE insertRaw #-}

lookupByID :: IF.CommentID -> AS.Query IF.ForumState (Maybe IF.Comment)
lookupByID cid = do
    db <- asks IF.stCommentDB
    return $ value <$> IX.getOne (db @= cid)

getThreadByCreation :: IF.PostID -> AS.Query IF.ForumState [IF.CommentEntity]
getThreadByCreation pid = do
    db <- asks IF.stCommentDB
    return $ IX.toList (db @= pid)
    
$(AS.makeEvents "events" ''IF.ForumState [ 'insert
                                         , 'insertRaw
                                         , 'lookupByID
                                         , 'getThreadByCreation
                                         ])

