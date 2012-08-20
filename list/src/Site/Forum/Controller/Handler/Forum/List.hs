module Site.Forum.Controller.Handler.Forum.List
( handler
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Server as HA
------------------------------------------------------------------------------
import qualified Web.Routes           as WR
------------------------------------------------------------------------------
import           Site.Common.Model
import           Site.Common.View.Template
------------------------------------------------------------------------------
import qualified Site.Forum.Route.Type              as IF
import qualified Site.Forum.View.Template.Forum     as IF.Forum
import qualified Site.Forum.Model.Forum             as IF.Forum
import qualified Site.Forum.Model                   as IF
------------------------------------------------------------------------------

handler :: IF.HasForum m
        => Page
        -> WR.RouteT IF.Route m HA.Response
handler p = do
    flist <- IF.Forum.getListPageByCreation p Increasing
    HA.ok =<< HA.toResponse <$> defaultTemplate (IF.Forum.templateList flist) 


