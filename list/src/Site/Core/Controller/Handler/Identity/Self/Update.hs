{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Core.Controller.Handler.Identity.Self.Update
( handler
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Server  as HA
import qualified Text.Reform.Extra as HA
------------------------------------------------------------------------------
import qualified Web.Routes           as WR
import qualified Web.Routes.Happstack as WR (seeOtherURL)
------------------------------------------------------------------------------
import           Site.Common.View.Template
------------------------------------------------------------------------------
import qualified Site.Core.Route.Type              as IC
import qualified Site.Core.Model                   as IC
import qualified Site.Core.Model.Profile           as IC.Profile
import qualified Site.Core.Controller.Form.Profile as IC.Profile
import qualified Site.Core.Model.Credentials       as IC.Credentials
import qualified Site.Core.View.Template.Identity  as IC
------------------------------------------------------------------------------

handler :: IC.HasCore m
        => WR.RouteT IC.Route m HA.Response
handler = do
    (iid, identity) <- IC.requireIdentity
    credentials <- IC.Credentials.lookupByIdentityID iid
    let form   = IC.Profile.updateForm $ IC.identityProfile identity
        exec p = IC.Profile.upsert iid p >> WR.seeOtherURL IC.Home
    view <- HA.reformURL IC.IdentitySelfUpdate exec Nothing form
    HA.ok =<< HA.toResponse <$> defaultTemplate (IC.templateUpdate view credentials)
