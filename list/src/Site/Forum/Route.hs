{-# LANGUAGE FlexibleContexts #-}

module Site.Forum.Route
( IF.Route(..)
, router
) where

------------------------------------------------------------------------------
import qualified Happstack.Server as HA
import qualified Happstack.State  as HA
------------------------------------------------------------------------------
import qualified Web.Routes as WR
------------------------------------------------------------------------------
import qualified Site.Forum.State as IF
import qualified Site.Forum.Route.Type as IF
import qualified Site.Forum.Controller.Handler.Home as IF.Home
------------------------------------------------------------------------------

router :: (HA.HasAcidState m IF.ForumState, HA.FilterMonad HA.Response m)
       => IF.Route -> WR.RouteT IF.Route m HA.Response
router (IF.Home) = IF.Home.handler