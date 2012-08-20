{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Happstack.Identity.Auth.Password.Default
( PasswordManager(..)
, initPasswordManager
, InsertPasswordBundle(..)
, LookupPassword(..) 
) where

------------------------------------------------------------------------------
import           Prelude
------------------------------------------------------------------------------
import           Control.Arrow (second)
import	      	 Control.Applicative
import           Control.Monad.Reader (asks)
import           Control.Monad.State  (get, put)
------------------------------------------------------------------------------
import qualified Data.Acid       as AS
import	      	 Data.Data
import           Data.Maybe
import qualified Data.SafeCopy   as SC
import           Data.IxSet               (IxSet, getOne, (@=), insert)
import qualified Data.IxSet      as IxSet
------------------------------------------------------------------------------
import qualified Happstack.Identity.Types               as I
import qualified Happstack.Identity.Auth.Password.Types as I
------------------------------------------------------------------------------

initPasswordManager :: PasswordManager
initPasswordManager = PasswordManager
    { nextPasswordID = toEnum 1
    , passwordStore  = IxSet.empty
    }

data PasswordManager = PasswordManager
    { nextPasswordID :: I.PasswordID
    , passwordStore  :: IxSet (I.PasswordID, I.PasswordBundle)
    } deriving (Eq, Ord, Typeable)
$(SC.deriveSafeCopy 0 'SC.base ''PasswordManager)

------------------------------------------------------------------------------
-- | PASSWORD STORE MANAGER IMPLEMENTATION BASED ON ACID-STATE

-- | Insert the given password bundle. If there already exists a bundle
-- with the given username, it does not insert it and returns Nothing.
insertPasswordBundle :: I.PasswordBundle
                     -> AS.Update PasswordManager (Maybe I.PasswordID)
insertPasswordBundle bundle@(I.PasswordBundle name _) = do
    state@PasswordManager{..} <- get 
    if isNothing $ getOne (passwordStore @= name)
       then do
         put state { nextPasswordID = succ nextPasswordID  
                   , passwordStore  = insert (nextPasswordID, bundle) passwordStore
                   }
         return $! Just nextPasswordID
       else return Nothing

lookupPassword :: I.PasswordHandle
               -> AS.Query PasswordManager (Maybe (I.PasswordID, I.PasswordSaltedHash))
lookupPassword name = do
    store <- asks passwordStore
    return $! second I.passwordSaltedHash <$> getOne (store @= name)


$(AS.makeAcidic ''PasswordManager [ 'insertPasswordBundle
                                  , 'lookupPassword
                                  ])

