{-# LANGUAGE TemplateHaskell #-}

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
           | ForumListPage Page
           | ForumListFrontPage
           | ForumReadPage      Path Page
           | ForumReadFrontPage Path
           | ForumCreate    
           | ForumCreateSub Path
           | PostReadFrontPage IF.PostID 
           | PostCreate
           | CommentCreate IF.PostID (Maybe IF.CommentID)

$(WR.derivePathInfo ''Route)
