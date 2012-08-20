{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Site.Core.Controller.Form.Auth.Password 
( formSignin
, formSignup
) where


------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Identity.Auth.Password as HA 
------------------------------------------------------------------------------
import qualified Text.Reform.Extra      as HA
import           Text.Reform                  ((<++), (++>))
import qualified Text.Reform.Blaze.Text as HA
import qualified Text.Blaze.Html as B (Html)
------------------------------------------------------------------------------
import qualified Data.Text          as TS (Text, null)
import qualified Data.Text.Encoding as TS (encodeUtf8)
------------------------------------------------------------------------------
import           Site.Common.Controller.Form
------------------------------------------------------------------------------

formSignin :: (Functor m, Monad m)
           => Form m B.Html () (HA.PasswordHandle, HA.Password) 
formSignin = signForm "Sign in"

formSignup :: (Functor m, Monad m)
           => Form m B.Html () (HA.PasswordHandle, HA.Password) 
formSignup = signForm "Sign up"

signForm :: (Functor m, Monad m)
         => TS.Text
         -> Form m B.Html () (HA.PasswordHandle, HA.Password) 
signForm submit = (,) 
     <$> HA.label ("Handle:" :: String)   ++> inputHandle    <++ HA.br
     <*> HA.label ("Password:" :: String) ++> inputPassword  <++ HA.br
     <*  HA.inputSubmit submit

inputHandle :: (Functor m, Monad m)
            => Form m B.Html () HA.PasswordHandle 
inputHandle = HA.PasswordHandle <$> 
    HA.checkBool (not . TS.null) (FERequiredLength 1 (-1)) (
    HA.inputText ""      
    )

inputPassword :: (Functor m, Monad m)
              => Form m B.Html () HA.Password 
inputPassword = HA.makePassword . TS.encodeUtf8 <$>
    HA.checkBool (not . TS.null) (FERequiredLength 1 (-1)) 
    HA.inputPassword
    



