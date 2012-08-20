{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}

module Site.Forum.Route
( IF.Route(..)
, router
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Server as HA
------------------------------------------------------------------------------
import qualified Web.Routes as WR
------------------------------------------------------------------------------
import           Site.Common.Model
------------------------------------------------------------------------------
import qualified Site.Core.Model as IC
import qualified Site.Forum.Model.State             as IF
import qualified Site.Forum.Route.Type              as IF
import qualified Site.Forum.Controller.Handler.Home as IF.Home
import qualified Site.Forum.Controller.Handler.Forum.List as IF.Forum.List
import qualified Site.Forum.Controller.Handler.Forum.Create as IF.Forum.Create
import qualified Site.Forum.Controller.Handler.Forum.Read as IF.Forum.Read
import qualified Site.Forum.Controller.Handler.Post.Create as IF.Post.Create
import qualified Site.Forum.Controller.Handler.Post.Read as IF.Post.Read
import qualified Site.Forum.Controller.Handler.Comment.Create as IF.Comment.Create
------------------------------------------------------------------------------

router :: (IC.HasCore m, IF.HasForum m)
       => IF.Route -> WR.RouteT IF.Route m HA.Response
router route =
    case route of
        (IF.Home) -> IF.Forum.List.handler (Page 1 20)
        (IF.ForumListPage p) -> IF.Forum.List.handler p
        (IF.ForumListFrontPage) -> IF.Forum.List.handler (Page 1 20)
        (IF.ForumReadPage path page) -> IF.Forum.Read.handler path page
        (IF.ForumReadFrontPage path) -> IF.Forum.Read.handler path (Page 1 20)
        (IF.ForumCreate) -> IF.Forum.Create.handler rootPath
        (IF.ForumCreateSub path) -> IF.Forum.Create.handler path
        (IF.PostReadFrontPage pid) -> IF.Post.Read.handler pid
        (IF.PostCreate) -> IF.Post.Create.handler
        (IF.CommentCreate pid mcid) -> IF.Comment.Create.handler pid mcid

