{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Forum.Controller.Handler.Post.Read
( handler 
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Server  as HA
------------------------------------------------------------------------------
import qualified Web.Routes           as WR
------------------------------------------------------------------------------
import           Site.Common.Model
import           Site.Common.View.Template
------------------------------------------------------------------------------
import qualified Site.Forum.Route.Type         as IF
import qualified Site.Forum.View.Template.Post as IF.Post
import qualified Site.Forum.Model.Type         as IF
import qualified Site.Forum.Model.Post         as IF.Post
import qualified Site.Forum.Model.Comment      as IF.Comment
------------------------------------------------------------------------------

handler :: IF.HasForum m
        => IF.PostID
        -> Page
        -> WR.RouteT IF.Route m HA.Response
handler pid _ = do
    epost <- requireExists
    comments <- IF.Comment.getThreadByCreation $ key epost
    HA.ok =<< HA.toResponse <$> defaultTemplate (IF.Post.templateRead epost comments) 
    where
      -- | Ensures that the Post exists.
      requireExists = 
        maybe mzero (\x -> return (pid, x)) =<< IF.Post.lookupByID pid
        
