{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Site.Core.Model.ACID.Credentials
( signinCredentials
, signupCredentials
, SigninCredentials(..)
, SignupCredentials(..)

, events

, module Export
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import           Control.Monad.Reader 
import           Control.Monad.State
------------------------------------------------------------------------------
import qualified Data.Acid     as AS
import           Data.IxSet 
------------------------------------------------------------------------------
import qualified Happstack.Identity.Types as HA
------------------------------------------------------------------------------
import           Site.Common.Model
import qualified Site.Core.Model.Type          as IC
import qualified Site.Core.Model.ACID.Identity as IC.Identity
import qualified Site.Core.Model.State         as IC
------------------------------------------------------------------------------
import           Site.Core.Model.Type.Credentials as Export
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- CREDENTIALS MANAGER IMPLEMENTATION USING ACID STATE

signinCredentials :: HA.Credentials
                  -> AS.Query IC.CoreState (Maybe IC.IdentityID) 
signinCredentials crd = do
    IC.CoreState{..} <- ask 
    return $! IC.credentialsIdentityID . snd <$> getOne (stCredentialsDB @= crd)    


signupCredentials :: HA.Credentials
                  -> AS.Update IC.CoreState (Maybe IC.IdentityID) 
signupCredentials crd = do
    st@IC.CoreState{..} <- get 
    if isNothing $ getOne $ stCredentialsDB @= crd    
       then do
         iid <- IC.Identity.insert IC.newIdentity
         put st { IC.stCredentialsDB = insert (stNextCredentialsID, IC.Credentials iid crd) stCredentialsDB
                , IC.stNextCredentialsID = nextID stNextCredentialsID
                }
         return $ Just iid
       else return Nothing  

$(AS.makeEvents "events" ''IC.CoreState [ 'signinCredentials
                                        , 'signupCredentials
                                        ])


