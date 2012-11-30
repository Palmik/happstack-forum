{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Core.Controller.Handler.Identity.Read
( handler
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Server as HA
-- import qualified Happstack.Identity.Auth.Password as HA
-- import qualified Text.Reform.Blaze.Common as HA
-- import qualified Text.Reform.Happstack as HA
------------------------------------------------------------------------------
import qualified Web.Routes           as WR
-- import qualified Web.Routes.Happstack as WR (seeOtherURL)
------------------------------------------------------------------------------
-- import qualified Text.Blaze.Html5 as B
------------------------------------------------------------------------------
import           Site.Common.View.Template
import qualified Site.Core.Route.Type             as IC
import qualified Site.Core.Model                  as IC
import qualified Site.Core.Model.Identity         as IC.Identity
import qualified Site.Core.View.Template.Identity as IC
------------------------------------------------------------------------------

handler :: IC.HasCore m
        => IC.IdentityID
        -> WR.RouteT IC.Route m HA.Response
handler iid = do
    midentity <- IC.Identity.lookupByID iid 
    HA.ok =<< HA.toResponse <$> defaultTemplate (IC.templateRead midentity)

