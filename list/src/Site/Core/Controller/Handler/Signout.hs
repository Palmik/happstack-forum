{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Core.Controller.Handler.Signout
( handler
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Server   as HA
import qualified Happstack.Identity as HA
------------------------------------------------------------------------------
import qualified Web.Routes           as WR
import qualified Web.Routes.Happstack as WR (seeOtherURL)
------------------------------------------------------------------------------
import qualified Site.Core.Route.Type  as IC
import qualified Site.Core.Model.State as IC
------------------------------------------------------------------------------

handler :: IC.HasCore m
        => WR.RouteT IC.Route m HA.Response
handler = HA.signout >> WR.seeOtherURL IC.Home

