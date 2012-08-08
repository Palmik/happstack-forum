{-# LANGUAGE FlexibleContexts #-}

module Site.Core.Route
( IC.Route(..)
, router
) where

------------------------------------------------------------------------------
import qualified Happstack.Server as HA
import qualified Happstack.State  as HA
------------------------------------------------------------------------------
import qualified Web.Routes as WR
------------------------------------------------------------------------------
import qualified Site.Core.State as IC
import qualified Site.Core.Route.Type as IC
import qualified Site.Core.Controller.Handler.Home as IC.Home
------------------------------------------------------------------------------

router :: (HA.HasAcidState m IC.CoreState, HA.FilterMonad HA.Response m)
       => IC.Route -> WR.RouteT IC.Route m HA.Response
router (IC.Home) = IC.Home.handler