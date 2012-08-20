{-# LANGUAGE OverloadedStrings #-}

module Main
( main
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Server as HA
------------------------------------------------------------------------------
import qualified Web.Routes.Happstack as WR
------------------------------------------------------------------------------
import qualified Site as I
------------------------------------------------------------------------------

main :: IO ()
main =
    I.withSiteState Nothing $! \state ->
        HA.simpleHTTP HA.nullConf $! do
            HA.decodeBody (HA.defaultBodyPolicy "/tmp/" 0 10000 10000)
            msum [ HA.dir "static" $! HA.serveDirectory HA.DisableBrowsing [] "static"
                 , I.runSite state $! WR.implSite "http://localhost:8000" "" $! I.site
                 ]
