{-# LANGUAGE TemplateHaskell #-}

module Site.Route.Type
( Route(..)
) where

------------------------------------------------------------------------------
import qualified Web.Routes.TH as WR
------------------------------------------------------------------------------
import qualified Site.Core.Route.Type  as IC
import qualified Site.Forum.Route.Type as IF
------------------------------------------------------------------------------

data Route = Core  IC.Route
           | Forum IF.Route

$(WR.derivePathInfo ''Route)

