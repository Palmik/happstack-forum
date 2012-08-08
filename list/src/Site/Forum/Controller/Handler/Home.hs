{-# LANGUAGE FlexibleContexts #-}

module Site.Forum.Controller.Handler.Home
( handler
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Server as HA
import qualified Happstack.State  as HA
------------------------------------------------------------------------------
import qualified Web.Routes           as WR
import qualified Web.Routes.Happstack as WR ()
------------------------------------------------------------------------------
import qualified Text.Blaze.Html as B
------------------------------------------------------------------------------
import           Site.Common.View.Template
import qualified Site.Forum.Route.Type as IF
import qualified Site.Forum.State      as IF
------------------------------------------------------------------------------

handler :: (HA.HasAcidState m IF.ForumState, HA.FilterMonad HA.Response m)
        => WR.RouteT IF.Route m HA.Response
handler = HA.ok $ HA.toResponse $ defaultTemplate $ DefaultTemplate
    { templateTitle = "Forum Home"
    , templateSectionHead = []
    , templateSectionL = [B.toHtml $ ("Left Sidebar" :: String)]
    , templateSectionM = [B.toHtml $ ("Forum Home" :: String)]
    , templateSectionR = [B.toHtml $ ("Right Sidebar" :: String)]
    }