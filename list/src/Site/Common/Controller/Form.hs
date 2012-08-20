{-# LANGUAGE TypeFamilies #-}

module Site.Common.Controller.Form
( Form
, Proof
, FormError(..)
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Data.Text as TS
------------------------------------------------------------------------------
import qualified Happstack.Server as HA
------------------------------------------------------------------------------
import qualified Text.Reform              as F
------------------------------------------------------------------------------

type Form  m view proof = F.Form  m [HA.Input] FormError view proof 
type Proof m      proof = F.Proof m FormError proof

data FormError = FECommon (F.CommonFormError [HA.Input])
               | FERequiredUnique
               | FERequiredLength Int Int
               | FENonexistentForums [TS.Text]

instance F.FormError FormError where
    type ErrorInputType FormError = [HA.Input]
    commonFormError = FECommon

