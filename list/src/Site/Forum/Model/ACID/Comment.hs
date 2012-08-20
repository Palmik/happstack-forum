{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Site.Forum.Model.ACID.Comment
( insert
, Insert(..)
, getThreadByCreation
, GetThreadByCreation(..)

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
import qualified Site.Forum.Model.State as IF
import qualified Site.Forum.Model.Type  as IF
------------------------------------------------------------------------------

insert :: IF.Comment -> AS.Update IF.ForumState IF.CommentID
insert comment = do
    st@IF.ForumState{..} <- get
    put st { IF.stNextCommentID = nextID stNextCommentID
           , IF.stCommentDB = IX.insert (stNextCommentID, comment) stCommentDB
           }
    return stNextCommentID

getThreadByCreation :: IF.PostID -> AS.Query IF.ForumState [IF.CommentEntity]
getThreadByCreation pid = do
    db <- asks IF.stCommentDB
    return $ IX.toList (db @= pid)
    
$(AS.makeEvents "events" ''IF.ForumState [ 'insert
                                         , 'getThreadByCreation
                                         ])

