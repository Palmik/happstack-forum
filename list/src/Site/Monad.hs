{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Site.Monad
( SiteState(..)
, Site
, withSiteState
, runSite
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import           Control.Monad.Reader
------------------------------------------------------------------------------
import qualified Happstack.Server   as HA
import qualified Happstack.State    as HA
import qualified Happstack.Identity as HA
import qualified Happstack.Identity.Auth.Password         as HA
import qualified Happstack.Identity.Auth.Password.Default as HA
------------------------------------------------------------------------------
import qualified Data.Acid          as AS
import qualified Data.Acid.Advanced as AS
------------------------------------------------------------------------------
import qualified Site.Core.Model                  as IC
import qualified Site.Core.Model.ACID.Session     as IC
import qualified Site.Core.Model.ACID.Session     as IC.Session
import qualified Site.Core.Model.ACID.Credentials as IC.Credentials
------------------------------------------------------------------------------
import qualified Site.Forum.Model as IF
------------------------------------------------------------------------------

data SiteState = SiteState
    { stateCore  :: AS.AcidState IC.CoreState
    , stateForum :: AS.AcidState IF.ForumState
    , stateSessionManager  :: AS.AcidState IC.SessionManager
    , statePasswordManager :: AS.AcidState HA.PasswordManager
    }

withSiteState :: Maybe FilePath -> (SiteState -> IO a) -> IO a
withSiteState mpath action =
    HA.withLocalState (Just $ path </> "core")  IC.initCoreState  $! \core ->
    HA.withLocalState (Just $ path </> "forum") IF.initForumState $! \forum ->
    HA.withLocalState (Just $ path </> "session_manager")  IC.initSessionManager  $! \smanager ->
    HA.withLocalState (Just $ path </> "password_manager") HA.initPasswordManager $! \pmanager ->
    action $! SiteState core forum smanager pmanager
    where
      path = fromMaybe "_state" mpath

newtype Site a = Site
    { unSite :: HA.ServerPartT (ReaderT SiteState IO) a
    } deriving ( Functor, Applicative, Alternative, Monad, MonadPlus
               , MonadReader SiteState, MonadIO
               , HA.HasRqData, HA.ServerMonad, HA.WebMonad HA.Response
               , HA.FilterMonad HA.Response, HA.Happstack
               )

runSite :: SiteState -> Site a -> HA.ServerPartT IO a
runSite state = HA.mapServerPartT (`runReaderT` state) . unSite

instance HA.HasAcidState Site IF.ForumState where
    getAcidState = asks stateForum

instance HA.HasAcidState Site IC.CoreState where
    getAcidState = asks stateCore

instance HA.HasIdentity Site where
    type IdentityID Site = IC.IdentityID

instance HA.HasIdentityManager Site where
    signinCredentials crd = do
        st <- asks stateCore
        AS.query' st $! IC.Credentials.Signin crd

    signupCredentials crd = do
        st <- asks stateCore
        AS.update' st $! IC.Credentials.Signup crd

    lookupSession skey = do
        st <- asks stateSessionManager
        AS.query' st $! IC.Session.Lookup skey

    insertSession skey s = do
        st <- asks stateSessionManager
        AS.update' st $! IC.Session.Insert skey s

    deleteSession skey = do
        st <- asks stateSessionManager
        AS.update' st $! IC.Session.Delete skey

instance HA.HasPasswordManager Site where
    insertPasswordBundle pb = do
        st <- asks statePasswordManager 
        AS.update' st $! HA.InsertPasswordBundle pb

    updatePasswordBundle pb = do
        st <- asks statePasswordManager 
        AS.update' st $! HA.UpdatePasswordBundle pb

    deletePasswordBundle handle = do
        st <- asks statePasswordManager 
        AS.update' st $! HA.DeletePasswordBundle handle

    lookupPassword ph = do
        st <- asks statePasswordManager
        AS.query' st $! HA.LookupPassword ph

instance IC.HasCore  Site where
instance IF.HasForum Site where


