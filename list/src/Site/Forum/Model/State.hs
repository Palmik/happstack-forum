{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Site.Forum.Model.State
( HasForum
, ForumState(..)
, initForumState
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.State
import           Control.Monad.RWS
------------------------------------------------------------------------------
import qualified Happstack.Server                 as HA
import qualified Happstack.State                  as HA
import qualified Happstack.Identity               as HA
import qualified Happstack.Identity.Auth.Password as HA
------------------------------------------------------------------------------
import qualified Web.Routes           as WR
import qualified Web.Routes.Happstack as WR ()
------------------------------------------------------------------------------
import qualified Data.IxSet    as IX
import qualified Data.SafeCopy as SC
------------------------------------------------------------------------------
import           Site.Common.Model
------------------------------------------------------------------------------
import qualified Site.Forum.Model.Type.Forum   as IF
import qualified Site.Forum.Model.Type.Post    as IF
import qualified Site.Forum.Model.Type.Comment as IF
------------------------------------------------------------------------------

class ( Show (HA.IdentityID m), HA.Happstack m, HA.HasPasswordManager m
      , HA.HasAcidState m ForumState
      ) => HasForum m where

instance (HasForum m)           => HasForum (StateT s m)
instance (HasForum m)           => HasForum (ReaderT r     m)
instance (HasForum m, Monoid w) => HasForum (WriterT   w   m)
instance (HasForum m, Monoid w) => HasForum (RWST    r w s m)
instance (HasForum m)           => HasForum (WR.RouteT url m)

data ForumState = ForumState
    { stNextForumID :: IF.ForumID
    , stForumDB     :: IX.IxSet IF.ForumEntity

    , stNextPostID :: IF.PostID
    , stPostDB     :: IX.IxSet IF.PostEntity

    , stNextCommentID :: IF.CommentID
    , stCommentDB     :: IX.IxSet IF.CommentEntity
    } deriving (Data, Eq, Ord, Typeable)

$(SC.deriveSafeCopy 0 'SC.base ''ForumState)

initForumState :: ForumState
initForumState = ForumState
    { stNextForumID = initID
    , stForumDB = IX.empty

    , stNextPostID = initID
    , stPostDB = IX.empty
    
    , stNextCommentID = initID
    , stCommentDB = IX.empty
    }

