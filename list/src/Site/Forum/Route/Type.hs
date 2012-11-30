{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Site.Forum.Route.Type
( Route(..)
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Web.Routes.TH as WR
------------------------------------------------------------------------------
import           Site.Common.Model
------------------------------------------------------------------------------
import qualified Site.Forum.Model.Type.Post    as IF
import qualified Site.Forum.Model.Type.Comment as IF
------------------------------------------------------------------------------

data Route = Home
           | ForumListRead Page
           
           | ForumRead Path Page
           | ForumCreate Path

           | PostRead IF.PostID Page
           | PostCreate

           | CommentCreate IF.PostID (Maybe IF.CommentID)

$(WR.derivePathInfo ''Route)
