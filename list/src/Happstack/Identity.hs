{-# LANGUAGE FlexibleContexts #-}

module Happstack.Identity
( maybeIdentityID
, signout

, module Export 
) where

------------------------------------------------------------------------------
import           Prelude
------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans (MonadIO(..))
------------------------------------------------------------------------------
import qualified Happstack.Server as HA
------------------------------------------------------------------------------
import qualified Happstack.Identity.Types        as I
import qualified Happstack.Identity.Auth.Session as I
------------------------------------------------------------------------------
import           Happstack.Identity.Types         as Export
import           Happstack.Identity.Auth.Session  as Export
------------------------------------------------------------------------------

signout :: ( Applicative m, MonadIO m
           , HA.HasRqData m, HA.ServerMonad m, HA.FilterMonad HA.Response m
           , I.HasIdentityManager m
           )
        => m ()
signout = do
    mcookie <- I.lookupSessionCookie
    case mcookie of
        Just (I.SessionCookie skey) -> do
          _ <- I.deleteSession skey
          I.deleteSessionCookie
        Nothing -> return ()

maybeIdentityID :: ( Applicative m, Monad m, I.HasIdentityManager m, MonadIO m
                   , HA.ServerMonad m, HA.FilterMonad HA.Response m, HA.HasRqData m
                   )
                => m (Maybe (I.IdentityID m))
maybeIdentityID = do
    mcookie <- I.lookupSessionCookie
    case mcookie of
        Just (I.SessionCookie skey) -> do
          mses <- I.lookupSession skey
          case mses of
              Just (I.Session iid) -> return $ Just iid
              Nothing -> return Nothing
        Nothing -> return Nothing      

