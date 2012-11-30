{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}

module Site.Forum.Route
( IF.Route(..)
, router
) where

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
        (IF.ForumListRead page) -> IF.Forum.List.handler page
        (IF.ForumRead path page) -> IF.Forum.Read.handler path page
        (IF.ForumCreate mpath) -> IF.Forum.Create.handler mpath
        (IF.PostRead pid page) -> IF.Post.Read.handler pid page
        (IF.PostCreate) -> IF.Post.Create.handler
        (IF.CommentCreate pid mcid) -> IF.Comment.Create.handler pid mcid

