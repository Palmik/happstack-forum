{-# LANGUAGE TemplateHaskell #-}

module Site.Core.Route.Type
( Route(..)
) where

------------------------------------------------------------------------------
import qualified Web.Routes.TH as WR
------------------------------------------------------------------------------
import qualified Site.Core.Model.Type.Credentials as IC
import qualified Site.Core.Model.Type.Identity    as IC
------------------------------------------------------------------------------

data Route = Home
           | Signin
           | Signup
           | Signout
           | CredentialsUpdate IC.CredentialsID
           | IdentityRead IC.IdentityID
           | IdentitySelfUpdate


$(WR.derivePathInfo ''Route)
