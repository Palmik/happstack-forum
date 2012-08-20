{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Site.Core.Model.ACID.Credentials
( signin
, signup
, Signin(..)
, Signup(..)

, events

, module Export
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import           Control.Monad.Reader
import           Control.Monad.State
------------------------------------------------------------------------------
import qualified Data.Acid  as AS
import qualified Data.IxSet as IX
import           Data.IxSet       ((@=))
------------------------------------------------------------------------------
import qualified Happstack.Identity.Types as HA
------------------------------------------------------------------------------
import           Site.Common.Model
import qualified Site.Core.Model.Type          as IC
import qualified Site.Core.Model.ACID.Identity as IC.Identity
------------------------------------------------------------------------------
import           Site.Core.Model.Type.Credentials as Export
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- CREDENTIALS MANAGER IMPLEMENTATION USING ACID STATE

signin :: HA.Credentials
       -> AS.Query IC.CoreState (Maybe IC.IdentityID)
signin crd = do
    IC.CoreState{..} <- ask
    return $! IC.credentialsIdentityID . snd <$> IX.getOne (stCredentialsDB @= crd)


signup :: HA.Credentials
       -> AS.Update IC.CoreState (Maybe IC.IdentityID)
signup crd = do
    unique <- not <$> AS.runQuery (exist crd)
    if unique
       then do
         iid <- IC.Identity.insert IC.newIdentity
         insert crd iid
         return $ Just iid
       else return Nothing

insert :: HA.Credentials -> IC.IdentityID -> AS.Update IC.CoreState ()
insert crd iid = do
    st@IC.CoreState{..} <- get
    put st { IC.stCredentialsDB = IX.insert (stNextCredentialsID, IC.Credentials iid crd) stCredentialsDB
           , IC.stNextCredentialsID = nextID stNextCredentialsID
           }
{-# INLINE insert #-}

exist :: HA.Credentials -> AS.Query IC.CoreState Bool
exist crd = do
    set <- asks IC.stCredentialsDB
    return $ isJust $ IX.getOne $ set @= crd
{-# INLINE exist #-}

$(AS.makeEvents "events" ''IC.CoreState [ 'signin
                                        , 'signup
                                        ])


 
