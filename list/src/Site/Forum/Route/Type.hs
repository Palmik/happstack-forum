{-# LANGUAGE TemplateHaskell #-}

module Site.Forum.Route.Type
( Route(..)
) where

------------------------------------------------------------------------------
import qualified Web.Routes.TH as WR
------------------------------------------------------------------------------

data Route = Home

$(WR.derivePathInfo ''Route)