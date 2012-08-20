{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeFamilies       #-}

module Site.Core.Model.ACID.Session 
( SessionManager(..)
, initSessionManager

, Insert(..)
, Lookup(..)
, Delete(..)
) where

------------------------------------------------------------------------------
import           Common
------------------------------------------------------------------------------
import           Control.Monad.Reader 
import           Control.Monad.State
------------------------------------------------------------------------------
import qualified Data.Acid     as AS
import qualified Data.SafeCopy as SC
import           Data.IxSet 
import qualified Data.IxSet    as IxSet
------------------------------------------------------------------------------
import qualified Happstack.Identity.Types as HA
------------------------------------------------------------------------------
import qualified Site.Core.Model.Type.Identity as IC
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- SESSION MANAGER IMPLEMENTATION USING ACID STATE

initSessionManager :: SessionManager
initSessionManager = SessionManager
    { sessions = IxSet.empty
    }

type Session = HA.Session IC.IdentityID

data SessionBundle = SessionBundle
    { sessionKey  :: HA.SessionKey
    , sessionData :: Session
    } deriving (Data, Eq, Ord, Typeable)

data SessionManager = SessionManager
    { sessions :: IxSet SessionBundle
    } deriving (Data, Eq, Ord, Typeable)

insert :: HA.SessionKey
       -> Session
       -> AS.Update SessionManager ()
insert skey s = do
    st@SessionManager{..} <- get
    put st { sessions =  updateIx skey (SessionBundle skey s) sessions }

lookup :: HA.SessionKey
       -> AS.Query SessionManager (Maybe Session)
lookup skey = do
    SessionManager{..} <- ask
    return $! sessionData <$> getOne (sessions @= skey) 

delete :: HA.SessionKey
       -> AS.Update SessionManager Bool
delete skey = do
    st@SessionManager{..} <- get
    put st { sessions = deleteIx skey sessions }
    return $! isJust $! getOne (sessions @= skey)

instance Indexable SessionBundle where
    empty = ixSet [ ixFun $ \e -> [ sessionKey e ] ]

$(AS.makeAcidic ''SessionManager [ 'insert
                                 , 'lookup
                                 , 'delete
                                 ])

$(SC.deriveSafeCopy 0 'SC.base ''SessionBundle)
$(SC.deriveSafeCopy 0 'SC.base ''SessionManager)

