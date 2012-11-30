{-# LANGUAGE NoImplicitPrelude #-}

module Site.Forum.Controller.Handler.Forum.Create
( handler
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Server  as HA
import qualified Text.Reform.Extra as HA
------------------------------------------------------------------------------
import qualified Web.Routes           as WR
import qualified Web.Routes.Happstack           as WR
------------------------------------------------------------------------------
import           Site.Common.Model
import           Site.Common.View.Template
------------------------------------------------------------------------------
import qualified Site.Forum.Route.Type             as IF
import qualified Site.Forum.View.Template.Forum    as IF.Forum
import qualified Site.Forum.Model.Forum            as IF.Forum
import qualified Site.Forum.Controller.Form.Forum  as IF.Forum
import qualified Site.Forum.Model                  as IF
------------------------------------------------------------------------------

handler :: IF.HasForum m
        => Path
        -> WR.RouteT IF.Route m HA.Response
handler ppath = do
    pfid <- requireParentID
    view <- HA.reformURL (IF.ForumCreate ppath) handleForm Nothing (IF.Forum.formCreate pfid)
    HA.ok =<< HA.toResponse <$> defaultTemplate (IF.Forum.templateCreate view)
    where
      handleForm fdata = do
        mpath <- fmap (IF.forumPath . snd) <$> IF.Forum.insert fdata
        case mpath of
            Just path -> WR.seeOtherURL $ IF.ForumRead path PageFront
            Nothing -> WR.seeOtherURL $ IF.ForumCreate ppath

      requireParentID = 
        case ppath of 
            PathRoot -> return Nothing
            p -> do
               mpar <- IF.Forum.lookupByPath p
               case mpar of
                   Just par -> return $ Just $ key par
                   Nothing  -> mzero

