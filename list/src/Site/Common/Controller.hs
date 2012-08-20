{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Site.Common.Controller
( seeOther
, seeReferrer

, module Export
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Server      as HA
import qualified Happstack.Server.SURI as HA
------------------------------------------------------------------------------
import qualified Web.Routes           as WR
import qualified Web.Routes.Happstack as WR
------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
------------------------------------------------------------------------------
import           Site.Core.Model  as Export ( maybeIdentity, maybeIdentityID
                                            , requireIdentity, requireIdentityID
                                            )
------------------------------------------------------------------------------

seeOther :: (WR.MonadRoute m, HA.FilterMonad HA.Response m)
         => WR.URL m
         -> m HA.Response
seeOther = WR.seeOtherURL
{-# INLINE seeOther #-}

seeReferrer :: (WR.MonadRoute m, HA.FilterMonad HA.Response m, HA.ServerMonad m)
           => WR.URL m
           -> m HA.Response
seeReferrer defuri = do
    req  <- HA.askRq
    case getReferer req of
        Just uri -> HA.seeOther uri $ HA.toResponse ("" :: String)
        Nothing  -> WR.seeOtherURL defuri
    where
      getReferer req = HA.toSURI <$> BS.unpack <$> HA.getHeader "referer" req 
      {-# INLINE getReferer #-}
{-# INLINE seeReferrer #-}
