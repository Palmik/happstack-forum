{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Site.Core.Controller.Form.Auth.Password 
( formSignin
, formSignup
, formUpdate
, formDelete
) where


------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Happstack.Identity               as HA 
import qualified Happstack.Identity.Auth.Password as HA 
------------------------------------------------------------------------------
import qualified Text.Reform.Extra      as HA
import           Text.Reform                  ((<++), (++>))
import qualified Text.Reform.Blaze.Text as HA
import qualified Text.Blaze.Html as B (Html)
------------------------------------------------------------------------------
import qualified Data.Text          as TS (Text, null, length)
import qualified Data.Text.Encoding as TS (encodeUtf8)
------------------------------------------------------------------------------
import           Site.Common.Controller.Form
------------------------------------------------------------------------------

formUpdate :: (Functor m, Monad m)
           => HA.PasswordSaltedHash
           -> Form m B.Html () (HA.Password, HA.Password)
formUpdate hash = (,) 
    <$> HA.label ("Current Password: " :: String) ++> inputPasswordVerify hash <++ HA.br
    <*> HA.label ("New Password: " :: String) ++> inputPassword <++ HA.br
    <*  HA.inputSubmit "Update"

formDelete :: (Functor m, Monad m)
           => HA.PasswordSaltedHash
           -> Form m B.Html () HA.Password
formDelete hash =
       HA.label ("Current Password: " :: String) ++> inputPasswordVerify hash <++ HA.br
    <* HA.inputSubmit "Delete"

formSignin :: (Functor m, Monad m)
           => Form m B.Html () (HA.PasswordHandle, HA.Password) 
formSignin = formSign "Sign in"

formSignup :: (Functor m, Monad m)
           => Form m B.Html () (HA.PasswordHandle, HA.Password) 
formSignup = formSign "Sign up"

formSign :: (Functor m, Monad m)
         => TS.Text
         -> Form m B.Html () (HA.PasswordHandle, HA.Password) 
formSign submit = (,) 
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
    HA.checkBool (\x -> let l = TS.length x in l >= 5 && l <= 200) (FERequiredLength 5 200) 
    HA.inputPassword
    
inputPasswordVerify :: (Functor m, Monad m)
                       => HA.PasswordSaltedHash
                       -> Form m B.Html () HA.Password 
inputPasswordVerify hash = HA.check verify HA.inputPassword
    where
      verify raw = 
        if HA.verifyPassword pass hash
           then Right pass
           else Left FEWrongPassword
        where
          pass = HA.makePassword $ TS.encodeUtf8 raw

