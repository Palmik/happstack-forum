{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

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
import           Control.Monad.Trans
------------------------------------------------------------------------------
import qualified Happstack.Server as HA
import qualified Happstack.State  as HA
------------------------------------------------------------------------------
import qualified Data.Acid as AS
------------------------------------------------------------------------------
import qualified Site.Core.State  as IC
import qualified Site.Forum.State as IF
------------------------------------------------------------------------------

data SiteState = SiteState
    { stateCore  :: AS.AcidState IC.CoreState
    , stateForum :: AS.AcidState IF.ForumState
    }

withSiteState :: Maybe FilePath -> (SiteState -> IO a) -> IO a
withSiteState mpath action =
    HA.withLocalState (Just $ path </> "core")  IC.initCoreState  $! \core  ->
    HA.withLocalState (Just $ path </> "forum") IF.initForumState $! \forum ->
    action $! SiteState core forum
    where path = fromMaybe "_state" mpath

newtype Site a = Site
    { unSite :: HA.ServerPartT (ReaderT SiteState IO) a
    } deriving ( Functor, Applicative, Alternative, Monad, MonadPlus
               , MonadReader SiteState, MonadIO
               , HA.HasRqData, HA.ServerMonad, HA.WebMonad HA.Response
               , HA.FilterMonad HA.Response, HA.Happstack
               )

runSite :: SiteState -> Site a -> HA.ServerPartT IO a
runSite state (Site sp) = HA.mapServerPartT (flip runReaderT state) sp

instance HA.HasAcidState Site IF.ForumState where
    getAcidState = asks stateForum

instance HA.HasAcidState Site IC.CoreState where
    getAcidState = asks stateCore
    