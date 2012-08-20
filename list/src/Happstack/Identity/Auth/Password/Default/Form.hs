{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Happstack.Identity.Auth.Password.Default.Form
( formSignin
, formSignup

, inputHandle
, inputPassword
) where


------------------------------------------------------------------------------
import           Control.Applicative
------------------------------------------------------------------------------
import qualified Text.Reform            as HA
import           Text.Reform                  ((<++), (++>))
import qualified Text.Reform.Blaze.Text as HA
import qualified Text.Blaze.Html as B (Html)
------------------------------------------------------------------------------
import qualified Data.Text          as TS (Text)
import qualified Data.Text.Encoding as TS (encodeUtf8)
------------------------------------------------------------------------------
import qualified Happstack.Identity.Auth.Password.Types as I
------------------------------------------------------------------------------


formSignin :: ( Functor m, Monad m, HA.FormInput input, HA.FormError error
              , HA.ErrorInputType error ~ input
              )
           => HA.Form m input error B.Html () (I.PasswordHandle, I.Password) 
formSignin = signForm "Sign in"

formSignup :: ( Functor m, Monad m, HA.FormInput input, HA.FormError error
              , HA.ErrorInputType error ~ input
              )
           => HA.Form m input error B.Html () (I.PasswordHandle, I.Password) 
formSignup = signForm "Sign up"

signForm :: ( Functor m, Monad m, HA.FormInput input, HA.FormError error
            , HA.ErrorInputType error ~ input
            )
         => TS.Text
         -> HA.Form m input error B.Html () (I.PasswordHandle, I.Password) 
signForm submit = (,) 
     <$> HA.label ("Handle:" :: String)   ++> inputHandle "" <++ HA.br
     <*> HA.label ("Password:" :: String) ++> inputPassword  <++ HA.br
     <*  HA.inputSubmit submit


inputHandle :: ( Functor m, Monad m, HA.FormInput input, HA.FormError error
               , HA.ErrorInputType error ~ input
               )
            => TS.Text
            -> HA.Form m input error B.Html () I.PasswordHandle 
inputHandle def = I.PasswordHandle <$> HA.inputText def


inputPassword :: ( Functor m, Monad m, HA.FormInput input, HA.FormError error
                 , HA.ErrorInputType error ~ input
                 )
              => HA.Form m input error B.Html () I.Password 
inputPassword = I.makePassword . TS.encodeUtf8 <$> HA.inputPassword

