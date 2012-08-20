{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}

module Site.Core.Route
( IC.Route(..)
, router
) where

------------------------------------------------------------------------------
import qualified Happstack.Server as HA
------------------------------------------------------------------------------
import qualified Web.Routes as WR
------------------------------------------------------------------------------
import qualified Site.Core.Model.State as IC
import qualified Site.Core.Route.Type as IC
import qualified Site.Core.Controller.Handler.Home    as IC.Home
import qualified Site.Core.Controller.Handler.Signin  as IC.Signin
import qualified Site.Core.Controller.Handler.Signup  as IC.Signup
import qualified Site.Core.Controller.Handler.Signout as IC.Signout
import qualified Site.Core.Controller.Handler.Identity.Read as IC.Identity.Read
import qualified Site.Core.Controller.Handler.Identity.Self.Update as IC.Identity.Self.Update
------------------------------------------------------------------------------

router :: IC.HasCore m
       => IC.Route -> WR.RouteT IC.Route m HA.Response
router (IC.Home)    = IC.Home.handler
router (IC.Signin)  = IC.Signin.handler
router (IC.Signup)  = IC.Signup.handler
router (IC.Signout) = IC.Signout.handler
router (IC.IdentityRead iid) = IC.Identity.Read.handler iid
router (IC.IdentitySelfUpdate) = IC.Identity.Self.Update.handler
