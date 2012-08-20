{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Site.Core.Model.ACID.Identity
( insert
, update
, upsert
, lookupByID
, lookupByHandle
, lookupIDByHandle

, Insert(..)
, Update(..)
, Upsert(..)
, LookupByID(..)
, LookupByHandle(..)
, LookupIDByHandle(..)

, events
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import           Control.Monad.State
import           Control.Monad.Reader
------------------------------------------------------------------------------
import qualified Data.Acid  as AS
import qualified Data.IxSet as IX  
import           Data.IxSet       ((@=))
import qualified Data.Text  as TS
------------------------------------------------------------------------------
import           Site.Common.Model
import qualified Site.Core.Model.Type  as IC
------------------------------------------------------------------------------

insert :: IC.Identity -> AS.Update IC.CoreState IC.IdentityID
insert i = do
    st@IC.CoreState{..} <- get
    put $ st { IC.stIdentityDB    = IX.insert (stNextIdentityID, i) stIdentityDB
             , IC.stNextIdentityID = nextID stNextIdentityID
             }
    return stNextIdentityID

update :: IC.IdentityID
       -> IC.Identity
       -> AS.Update IC.CoreState Bool
update iid updated = do
    st@IC.CoreState{..} <- get
    mi <- AS.runQuery $ lookupByID iid
    case mi of
        Just _  -> do
          put st { IC.stIdentityDB = IX.updateIx iid (iid, updated) stIdentityDB }
          return True
        Nothing -> return True

upsert :: IC.IdentityID
       -> IC.Identity
       -> AS.Update IC.CoreState ()
upsert iid updated = do
    st@IC.CoreState{..} <- get
    put st { IC.stIdentityDB = IX.updateIx iid (iid, updated) stIdentityDB }

lookupByID :: IC.IdentityID -> AS.Query IC.CoreState (Maybe IC.Identity)
lookupByID iid = do
    set <- asks IC.stIdentityDB
    return $ snd <$> IX.getOne (set @= iid)

lookupByHandle :: TS.Text -> AS.Query IC.CoreState (Maybe IC.IdentityEntity)
lookupByHandle handle = do
    set <- asks IC.stIdentityDB
    return $ IX.getOne $ set @= handle

lookupIDByHandle :: TS.Text -> AS.Query IC.CoreState (Maybe IC.IdentityID)
lookupIDByHandle handle = do
    set <- asks IC.stIdentityDB
    return $ fst <$> IX.getOne (set @= handle)

$(AS.makeEvents "events" ''IC.CoreState [ 'insert
                                        , 'update
                                        , 'upsert
                                        , 'lookupByID
                                        , 'lookupByHandle
                                        , 'lookupIDByHandle
                                        ])

