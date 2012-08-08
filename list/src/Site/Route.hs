module Site.Route
( I.Route(..)
, router
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Server as HA
------------------------------------------------------------------------------
import qualified Web.Routes as WR
------------------------------------------------------------------------------
import qualified Site.Monad       as I
import qualified Site.Route.Type  as I
import qualified Site.Core.Route  as IC
import qualified Site.Forum.Route as IF
------------------------------------------------------------------------------

router :: I.Route -> WR.RouteT I.Route I.Site HA.Response
router (I.Core  r) = WR.nestURL I.Core  $ IC.router r
router (I.Forum r) = WR.nestURL I.Forum $ IF.router r
