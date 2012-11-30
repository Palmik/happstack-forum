{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Site.Forum.Model.ACID.Post
( insert
, Insert(..)
, lookupByID
, LookupByID(..)

, PostDataIO(..)

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

data PostDataIO = PostDataIO
    { iopostCreated :: UTCTime
    } deriving (Data, Eq, Ord, Typeable, Show)
$(SC.deriveSafeCopy 0 'SC.base ''PostDataIO)

makeComplete :: PostDataIO ->  IF.PostData -> AS.Query IF.ForumState IF.Post
makeComplete PostDataIO{..} pdata = return IF.Post
    { IF.postData = pdata
    , IF.postDataExtra = IF.PostDataExtra
        { IF.postCreated = iopostCreated
        }
    } 

insert :: PostDataIO -> IF.PostData -> AS.Update IF.ForumState IF.PostID
insert pdataio pdata = do
    post <- AS.runQuery $ makeComplete pdataio pdata
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

