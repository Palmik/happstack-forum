{-# LANGUAGE FlexibleContexts #-}

module Site.Core.Controller.Handler.Home
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
import qualified Site.Core.State      as IC
import qualified Site.Core.Route.Type as IC
------------------------------------------------------------------------------


handler :: (HA.HasAcidState m IC.CoreState, HA.FilterMonad HA.Response m)
        => WR.RouteT IC.Route m HA.Response
handler = HA.ok $ HA.toResponse $ defaultTemplate $ DefaultTemplate
    { templateTitle = "Site Home"
    , templateSectionHead = []
    , templateSectionL = ["Left Sidebar"]
    , templateSectionM = ["Site Home"]
    , templateSectionR = ["Right Sidebar"]
    }