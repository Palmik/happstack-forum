module Site.Forum.Controller.Handler.Post.Create
( handler
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import           Control.Monad.Trans (MonadIO(..))
------------------------------------------------------------------------------
import qualified Happstack.Server  as HA
import qualified Text.Reform.Extra as HA
------------------------------------------------------------------------------
import qualified Web.Routes           as WR
import qualified Web.Routes.Happstack as WR
------------------------------------------------------------------------------
import           Site.Common.Model
import           Site.Common.Controller
import           Site.Common.View.Template
------------------------------------------------------------------------------
import qualified Site.Core.Model as IC
import qualified Site.Forum.Route.Type              as IF
import qualified Site.Forum.View.Template.Post      as IF.Post
import qualified Site.Forum.Controller.Form.Post    as IF.Post
import qualified Site.Forum.Model.Post              as IF.Post
import qualified Site.Forum.Model                   as IF
------------------------------------------------------------------------------

handler :: (IC.HasCore m, IF.HasForum m)
        => WR.RouteT IF.Route m HA.Response
handler = do
    miid <- IC.maybeIdentityID
    time <- liftIO getCurrentTime
    view <- HA.reformURL IF.PostCreate handle Nothing $ IF.Post.formCreate miid time
    HA.ok =<< HA.toResponse <$> defaultTemplate (IF.Post.templateCreate view)
    where
      handle post = IF.Post.insert post >> WR.seeOtherURL IF.Home

