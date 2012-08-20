{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Core.Controller.Handler.Home
( handler
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Server   as HA
------------------------------------------------------------------------------
import qualified Web.Routes           as WR
import qualified Web.Routes.Happstack as WR ()
------------------------------------------------------------------------------
import           Site.Common.View.Template
import qualified Site.Core.Model.State as IC
import qualified Site.Core.Route.Type  as IC
------------------------------------------------------------------------------


handler :: IC.HasCore m
        => WR.RouteT IC.Route m HA.Response
handler = 
    HA.ok =<< HA.toResponse <$> defaultTemplate DefaultTemplate
      { templateTitle = "Site Home"
      , templateSectionHead = []
      , templateSectionL = ["Left Sidebar"]
      , templateSectionM = ["Site Home"]
      , templateSectionR = ["Right Sidebar"]
      }
