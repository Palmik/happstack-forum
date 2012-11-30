module Site.Forum.Controller.Handler.Comment.Create
( handler
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Server  as HA
import qualified Text.Reform.Extra as HA
------------------------------------------------------------------------------
import qualified Web.Routes           as WR
import qualified Web.Routes.Happstack as WR
------------------------------------------------------------------------------
import           Site.Common.View.Template
------------------------------------------------------------------------------
import qualified Site.Core.Model as IC
import qualified Site.Forum.Route.Type              as IF
import qualified Site.Forum.View.Template.Comment   as IF.Comment
import qualified Site.Forum.Controller.Form.Comment as IF.Comment
import qualified Site.Forum.Model.Comment           as IF.Comment
import qualified Site.Forum.Model                   as IF
------------------------------------------------------------------------------

handler :: (IC.HasCore m, IF.HasForum m)
        => IF.PostID
        -> Maybe IF.CommentID
        -> WR.RouteT IF.Route m HA.Response
handler pid mparent = do
    miid <- IC.maybeIdentityID
    view <- HA.reformURL (IF.CommentCreate pid mparent) handle Nothing $ IF.Comment.formCreate miid pid mparent
    HA.ok =<< HA.toResponse <$> defaultTemplate (IF.Comment.templateCreate view)
    where
      handle comment = IF.Comment.insert comment >> WR.seeOtherURL IF.Home


