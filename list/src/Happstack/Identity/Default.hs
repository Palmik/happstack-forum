{-
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeFamilies       #-}

module Happstack.Identity.Default
( SessionManager(..)
, SessionBundle(..)
) where

------------------------------------------------------------------------------
import           Prelude
------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Reader 
import           Control.Monad.State
------------------------------------------------------------------------------
import qualified Data.Acid     as AS
import           Data.Data
import qualified Data.SafeCopy as SC
import           Data.Maybe
import           Data.IxSet
------------------------------------------------------------------------------
import qualified Happstack.Identity.Types        as I
-- import qualified Happstack.Identity.Auth.Session as I
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- SESSION MANAGER IMPLEMENTATION USING ACID STATE

data SessionBundle a = SessionBundle
    { sessionKey  :: I.SessionKey
    , sessionData :: I.Session a
    } deriving (Data, Eq, Ord, Typeable)

data SessionManager a = SessionManager
    { sessions :: IxSet (SessionBundle a)
    } deriving (Data, Eq, Ord, Typeable)

insertSession :: (Eq a, Ord a, Typeable a)
              => I.SessionKey
              -> I.Session a
              -> AS.Update (SessionManager a) ()
insertSession skey s = do
    st@SessionManager{..} <- get
    put st { sessions =  updateIx skey (SessionBundle skey s) sessions }

lookupSession :: (Eq a, Ord a, Typeable a)
              => I.SessionKey
              -> AS.Query (SessionManager a) (Maybe (I.Session a))
lookupSession skey = do
    SessionManager{..} <- ask
    return $! sessionData <$> getOne (sessions @= skey) 

deleteSession :: (Eq a, Ord a, Typeable a)
              => I.SessionKey
              -> AS.Update (SessionManager a) Bool
deleteSession skey = do
    st@SessionManager{..} <- get
    put st { sessions = deleteIx skey sessions }
    return $! isJust $! getOne (sessions @= skey)

instance Indexable (SessionBundle a) where
    empty = ixSet [ ixFun $ \e -> [ sessionKey e ] ]

-- $(AS.makeAcidic ''SessionManager [ 'insertSession
--                                  , 'lookupSession
--                                  , 'deleteSession
--                                  ])

-- $(SC.deriveSafeCopy 0 'SC.base ''SessionBundle)

-- $(SC.deriveSafeCopy 0 'SC.base ''SessionManager)

-}
