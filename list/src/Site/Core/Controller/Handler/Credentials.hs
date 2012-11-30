module Site.Core.Controller.Handler.Credentials
( handlerUpdate
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Server                 as HA
import qualified Happstack.Identity               as HA
import qualified Happstack.Identity.Auth.Password as HA
import qualified Text.Reform.Extra                as HA (reformURL)
------------------------------------------------------------------------------
import qualified Web.Routes as WR
import qualified Web.Routes.Happstack as WR
------------------------------------------------------------------------------
import           Site.Common.View.Template
------------------------------------------------------------------------------
import qualified Site.Core.Route.Type                    as IC
import qualified Site.Core.Model                         as IC
import qualified Site.Core.Model.Credentials             as IC.Credentials
import qualified Site.Core.Controller.Form.Auth.Password as IC.Password
import qualified Site.Core.View.Template.Password as IC.Password
------------------------------------------------------------------------------

handlerUpdate :: IC.HasCore m
              => IC.CredentialsID
              -> WR.RouteT IC.Route m HA.Response
handlerUpdate cid = do
    iid <- IC.requireIdentityID
    mcreds <- IC.Credentials.lookupByID cid -- ^ Lookup the credentials we wish to update.
    case mcreds of
        -- | The credentials exist.
        Just (IC.Credentials ciid (HA.CredentialsPassword handle))
          -- | And the credentials' IID matches the users' IID.
          | ciid == iid -> do
              mhash <- HA.lookupPassword handle
              case mhash of
                  Just hash -> do
                    view <- HA.reformURL (IC.CredentialsUpdate cid)
                                         (handleFormPass handle)
                                         Nothing
                                         (IC.Password.formUpdate hash)
                    HA.ok =<< HA.toResponse <$> defaultTemplate (IC.Password.templateUpdate view)
                  Nothing -> mzero
          -- | But the credentials' IID does not match the users' IID.
          | otherwise -> mzero
        -- | The credentials do not exist, this should not happen.
        Nothing -> mzero
    where
      handleFormPass handle (old, new) =
        HA.update handle old new >> WR.seeOtherURL IC.IdentitySelfUpdate

