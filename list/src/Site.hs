module Site
( site
, I.runSite
, I.withSiteState
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Server as HA
------------------------------------------------------------------------------
import qualified Web.Routes as WR
------------------------------------------------------------------------------
import qualified Site.Route      as I
import qualified Site.Monad      as I
import qualified Site.Core.Route as IC
------------------------------------------------------------------------------

site :: WR.Site I.Route (I.Site HA.Response)
site = WR.setDefault (I.Core IC.Home) $ WR.mkSitePI (WR.runRouteT I.router)