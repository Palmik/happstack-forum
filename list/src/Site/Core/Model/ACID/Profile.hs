{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Site.Core.Model.ACID.Profile
( insert
, update
, upsert
, delete

, Insert(..)
, Update(..)
, Upsert(..)
, Delete(..)

, events

, module Export
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import qualified Data.Acid  as AS
------------------------------------------------------------------------------
import qualified Site.Core.Model.Type          as IC
import qualified Site.Core.Model.ACID.Identity as IC.Identity
------------------------------------------------------------------------------
import           Site.Core.Model.Type.Identity as Export
import           Site.Core.Model.Type.Profile  as Export
------------------------------------------------------------------------------

insert :: IC.IdentityID
       -> IC.Profile
       -> AS.Update IC.CoreState ()
insert iid profile = do
    mi <- AS.runQuery $ IC.Identity.lookupByID iid
    case mi of
        Just i  -> void $ IC.Identity.update iid $ upi i
        Nothing -> return ()
    where
      upi i =
        if isNothing $ IC.identityProfile i
           then i { IC.identityProfile = Just profile }
           else i

update :: IC.IdentityID
       -> IC.Profile
       -> AS.Update IC.CoreState Bool 
update iid profile = do 
    mi <- AS.runQuery $ IC.Identity.lookupByID iid
    case mi of
        Just i  -> IC.Identity.update iid $ upi i
        Nothing -> return False
    where
      upi i = i { IC.identityProfile = Just profile }

upsert :: IC.IdentityID
       -> IC.Profile
       -> AS.Update IC.CoreState ()
upsert iid profile = do
    mi <- AS.runQuery $ IC.Identity.lookupByID iid
    case mi of
        Just i  -> IC.Identity.upsert iid $ upi i
        Nothing -> return ()
    where
      upi i = i { IC.identityProfile = Just profile }

delete :: IC.IdentityID
       -> AS.Update IC.CoreState ()
delete iid = do 
    mi <- AS.runQuery $ IC.Identity.lookupByID iid
    case mi of
        Just i  -> void $ IC.Identity.update iid $ upi i
        Nothing -> return ()
    where
      upi i = i { IC.identityProfile = Nothing }


$(AS.makeEvents "events" ''IC.CoreState [ 'insert
                                        , 'update
                                        , 'upsert
                                        , 'delete
                                        ])


