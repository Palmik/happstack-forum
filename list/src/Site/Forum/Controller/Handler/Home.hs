{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Forum.Controller.Handler.Home
( handler
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Server as HA
------------------------------------------------------------------------------
import qualified Web.Routes           as WR
import qualified Web.Routes.Happstack as WR ()
------------------------------------------------------------------------------
-- import qualified Text.Blaze.Html as B
------------------------------------------------------------------------------
import           Site.Common.View.Template
import qualified Site.Forum.Route.Type  as IF
import qualified Site.Forum.Model.State as IF
------------------------------------------------------------------------------

handler :: IF.HasForum m 
        => WR.RouteT IF.Route m HA.Response
handler = HA.ok =<< HA.toResponse <$> defaultTemplate DefaultTemplate
    { templateTitle = "Forum Home"
    , templateSectionHead = []
    , templateSectionL = ["Left Sidebar"]
    , templateSectionM = ["Forum Home"]
    , templateSectionR = ["Right Sidebar"]
    }
