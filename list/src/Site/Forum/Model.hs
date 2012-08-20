module Site.Forum.Model
( module Export
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Data.Acid          as AS
import qualified Data.Acid.Advanced as AS
------------------------------------------------------------------------------
import qualified Site.Forum.Model.State        as IF
import qualified Site.Forum.Model.ACID.Forum   as IF.Forum
import qualified Site.Forum.Model.ACID.Post    as IF.Post
import qualified Site.Forum.Model.ACID.Comment as IF.Comment
------------------------------------------------------------------------------
import           Site.Forum.Model.Type as Export
------------------------------------------------------------------------------

instance AS.IsAcidic IF.ForumState where
    acidEvents = IF.Forum.events <>
                 IF.Post.events <>
                 IF.Comment.events
