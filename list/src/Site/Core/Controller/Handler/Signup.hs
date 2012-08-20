{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Core.Controller.Handler.Signup
( handler
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Server                 as HA
import qualified Happstack.Identity.Auth.Password as HA
import qualified Text.Reform.Extra                as HA (reformURL)
------------------------------------------------------------------------------
import qualified Web.Routes           as WR
import qualified Web.Routes.Happstack as WR (seeOtherURL)
------------------------------------------------------------------------------
import qualified Text.Blaze.Html5 as B
------------------------------------------------------------------------------
import           Site.Common.View.Template
------------------------------------------------------------------------------
import qualified Site.Core.Route.Type  as IC
import qualified Site.Core.Model.State as IC
import qualified Site.Core.Controller.Form.Auth.Password as IC
------------------------------------------------------------------------------

handler :: IC.HasCore m
        => WR.RouteT IC.Route m HA.Response
handler = do
    view  <- HA.reformURL IC.Signup handle Nothing IC.formSignup 
    HA.ok =<< HA.toResponse <$> defaultTemplate DefaultTemplate
      { templateTitle = "Site Home"
      , templateSectionHead = []
      , templateSectionL = ["Left Sidebar"]
      , templateSectionM = [B.h2 "Sign up", view]
      , templateSectionR = ["Right Sidebar"]
      }
    where
      handle res = uncurry HA.signup res >> WR.seeOtherURL IC.Home

