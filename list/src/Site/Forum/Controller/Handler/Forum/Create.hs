module Site.Forum.Controller.Handler.Forum.Create
( handler
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import           Control.Monad.Trans
------------------------------------------------------------------------------
import qualified Happstack.Server  as HA
import qualified Text.Reform.Extra as HA
------------------------------------------------------------------------------
import qualified Web.Routes           as WR
import qualified Web.Routes.Happstack           as WR
------------------------------------------------------------------------------
import           Site.Common.Model
import           Site.Common.Controller
import           Site.Common.View.Template
------------------------------------------------------------------------------
import qualified Site.Forum.Route.Type             as IF
import qualified Site.Forum.View.Template.Forum    as IF.Forum
import qualified Site.Forum.Model.Forum            as IF.Forum
import qualified Site.Forum.Controller.Form.Forum  as IF.Forum
import qualified Site.Forum.Model                  as IF
------------------------------------------------------------------------------

handler :: IF.HasForum m
        => Path
        -> WR.RouteT IF.Route m HA.Response
handler pp = do
    pfid <- requireParentID
    time <- liftIO getCurrentTime
    view <- HA.reformURL routee handle Nothing (IF.Forum.formCreate pfid time)
    HA.ok =<< HA.toResponse <$> defaultTemplate (IF.Forum.templateCreate view)
    where
      handle forum  = do
          _ <- IF.Forum.insert forum
          WR.seeOtherURL IF.ForumListFrontPage
      routee =
        if pp /= rootPath
           then IF.ForumCreateSub pp
           else IF.ForumCreate

      requireParentID = 
        if pp /= rootPath
           then do
               mpar <- IF.Forum.lookupByPath pp
               case mpar of
                   Just par -> return $ Just $ key par
                   Nothing  -> mzero
           else return Nothing

