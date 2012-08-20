{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Site.Core.Controller.Form.Profile 
( updateForm
) where


------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Text.Reform.Extra      as HA
import           Text.Reform                  ((<++), (++>))
import qualified Text.Reform.Blaze.Text as HA
import qualified Text.Blaze.Html as B (Html)
------------------------------------------------------------------------------
import qualified Data.Text          as TS (Text, null)
------------------------------------------------------------------------------
import           Site.Common.Controller.Form
------------------------------------------------------------------------------
import qualified Site.Core.Model.Type.Profile  as IC
------------------------------------------------------------------------------

updateForm :: (Functor m, Monad m)
           => Maybe IC.Profile
           -> Form m B.Html () IC.Profile 
updateForm mprofile = IC.Profile
    <$> HA.label ("Profile handle: " :: String)
          ++> profileHandle (IC.profileHandle <$> mprofile)
          <++ HA.br
    <*  HA.inputSubmit "Update"

profileHandle :: (Functor m, Monad m)
              => Maybe TS.Text
              -> Form m B.Html () TS.Text
profileHandle mdef = HA.checkBool (not . TS.null) (FERequiredLength 1 (-1)) $
    HA.inputText value
    where
      value = fromMaybe ""  mdef


