{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Site.Forum.Model.ACID.Post
( insert
, Insert(..)
, lookupByID
, LookupByID(..)

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

insert :: IF.Post -> AS.Update IF.ForumState IF.PostID
insert post = do
    st@IF.ForumState{..} <- get
    put st { IF.stNextPostID = nextID stNextPostID
           , IF.stPostDB = IX.insert (stNextPostID, post) stPostDB
           }
    return stNextPostID

lookupByID :: IF.PostID -> AS.Query IF.ForumState (Maybe IF.Post)
lookupByID pid = do
    db <- asks IF.stPostDB
    return $ value <$> IX.getOne (db @= pid)

$(AS.makeEvents "events" ''IF.ForumState [ 'insert
                                         , 'lookupByID
                                         ])

