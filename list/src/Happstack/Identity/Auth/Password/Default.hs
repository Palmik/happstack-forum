{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Happstack.Identity.Auth.Password.Default
( PasswordManager(..)
, initPasswordManager

, InsertPasswordBundle(..)
, UpdatePasswordBundle(..)
, DeletePasswordBundle(..)
, LookupPassword(..)
) where

------------------------------------------------------------------------------
import           Prelude
------------------------------------------------------------------------------
import	      	 Control.Applicative
import           Control.Monad.Reader (asks)
import           Control.Monad.State  (get, put)
------------------------------------------------------------------------------
import qualified Data.Acid       as AS
import	      	 Data.Data
import           Data.Maybe
import qualified Data.SafeCopy   as SC
import           Data.IxSet               (IxSet, getOne, (@=), insert, updateIx, deleteIx)
import qualified Data.IxSet      as IxSet
------------------------------------------------------------------------------
import qualified Happstack.Identity.Types               as I
import qualified Happstack.Identity.Auth.Password.Types as I
------------------------------------------------------------------------------

initPasswordManager :: PasswordManager
initPasswordManager = PasswordManager
    { passwordStore  = IxSet.empty
    }

newtype PasswordManager = PasswordManager
    { passwordStore  :: IxSet I.PasswordBundle
    } deriving (Eq, Ord, Typeable)
$(SC.deriveSafeCopy 0 'SC.base ''PasswordManager)

------------------------------------------------------------------------------
-- | PASSWORD STORE MANAGER IMPLEMENTATION BASED ON ACID-STATE

-- | Insert the given password bundle. If there already exists a bundle
-- with the given username, it's no-op and returns False, otherwise returns True.
insertPasswordBundle :: I.PasswordBundle
                     -> AS.Update PasswordManager Bool 
insertPasswordBundle bundle@(I.PasswordBundle handle _) = do
    st@PasswordManager{..} <- get 
    if isNothing $ getOne (passwordStore @= handle)
       then do
         put st { passwordStore  = insert bundle passwordStore }
         return True
       else return False

-- | Update the given password bundle. If there is no bundle with such
-- handle already, it's no-op and returns False, othwerise returns True.
updatePasswordBundle :: I.PasswordBundle 
                     -> AS.Update PasswordManager Bool
updatePasswordBundle bundle@(I.PasswordBundle handle _) = do
    st@PasswordManager{..} <- get
    if isJust $ getOne (passwordStore @= handle)
        then do
          put st { passwordStore = updateIx handle bundle passwordStore }
          return True 
        else return False

-- | Deletes bundle associated with the given handle. If there is no bundle with such
-- handle, it's no-op and returns False, othwerise returns True.
deletePasswordBundle :: I.PasswordHandle
                     -> AS.Update PasswordManager Bool
deletePasswordBundle handle = do
    st@PasswordManager{..} <- get
    if isJust $ getOne (passwordStore @= handle)
        then do
          put st { passwordStore = deleteIx handle passwordStore }
          return True 
        else return False


lookupPassword :: I.PasswordHandle
               -> AS.Query PasswordManager (Maybe I.PasswordSaltedHash)
lookupPassword handle = do
    db <- asks passwordStore
    return $! I.passwordSaltedHash <$> getOne (db @= handle)

$(AS.makeAcidic ''PasswordManager [ 'insertPasswordBundle
                                  , 'updatePasswordBundle
                                  , 'deletePasswordBundle
                                  , 'lookupPassword
                                  ])

