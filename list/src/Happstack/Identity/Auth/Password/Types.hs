{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Happstack.Identity.Auth.Password.Types
( HasPasswordManager(..)
, PasswordBundle(..)

  -- * Convenience wrappers
, Password
, PasswordSaltedHash
, makePassword
, makePasswordSaltedHash
, makePasswordBundle
, verifyPassword
) where

------------------------------------------------------------------------------
import           Prelude
------------------------------------------------------------------------------
import	      	 Control.Applicative
import		       Control.Monad.Trans (MonadTrans(..), MonadIO(..))
------------------------------------------------------------------------------
import		       Data.Data
import qualified Data.SafeCopy   as SC
import qualified Data.ByteString as BS
import           Data.IxSet            (Indexable(..), ixSet, ixFun)
------------------------------------------------------------------------------
import qualified Crypto.PasswordStore as PS
------------------------------------------------------------------------------
import qualified Happstack.Identity.Types as I
------------------------------------------------------------------------------

class I.HasIdentityManager m => HasPasswordManager m where
    -- | Insert the given password bundle. If there already exists a bundle
    -- with the given username, it's no-op and returns False, otherwise returns True.
    insertPasswordBundle :: PasswordBundle -> m Bool 
    
    -- | Update the given password bundle. If there is no bundle with such
    -- handle already, it's no-op and returns False, othwerise returns True.
    updatePasswordBundle :: PasswordBundle -> m Bool 
    
    -- | Deletes bundle associated with the given handle. If there is no bundle with such
    -- handle, it's no-op and returns False, othwerise returns True.
    deletePasswordBundle :: I.PasswordHandle -> m Bool 
    
    lookupPassword :: I.PasswordHandle -> m (Maybe PasswordSaltedHash)     

instance (Monad m, HasPasswordManager m, MonadTrans mt) => HasPasswordManager (mt m) where
    insertPasswordBundle = lift . insertPasswordBundle
    updatePasswordBundle = lift . updatePasswordBundle
    deletePasswordBundle = lift . deletePasswordBundle
    lookupPassword = lift . lookupPassword

data PasswordBundle = PasswordBundle
    { passwordHandle     :: I.PasswordHandle
    , passwordSaltedHash :: PasswordSaltedHash
    } deriving (Eq, Ord, Typeable)

instance Indexable PasswordBundle where
    empty = ixSet
      [ ixFun $ \e -> [ passwordHandle e ]
      ]

------------------------------------------------------------------------------
-- | CONVENIENCE FUNCTIONS AND TYPES (simple wrappers)

makePasswordBundle :: (Applicative m, MonadIO m)
		               => I.PasswordHandle 
		               -> Password 
                   -> m PasswordBundle 
makePasswordBundle name pass = PasswordBundle name <$> makePasswordSaltedHash pass 14

newtype Password = Password BS.ByteString
  deriving (Eq, Ord, Typeable)
  
newtype PasswordSaltedHash = PasswordSaltedHash BS.ByteString
  deriving (Eq, Ord, Typeable)

-- | Wraps the bytestring in the `Password` constructor.
makePassword :: BS.ByteString -> Password
makePassword = Password

-- | Equivalent to `Crypto.PasswordStore.makePassword`
makePasswordSaltedHash :: (Functor m, MonadIO m)
		                   => Password
                       -> Int
                       -> m PasswordSaltedHash
makePasswordSaltedHash (Password pass) str = PasswordSaltedHash <$> liftIO (PS.makePassword pass str)

-- | Equivalent to `Crypto.PasswordStore.verifyPassword`
verifyPassword :: Password -> PasswordSaltedHash -> Bool
verifyPassword (Password pass) (PasswordSaltedHash hash) = PS.verifyPassword pass hash

$(SC.deriveSafeCopy 0 'SC.base ''PasswordSaltedHash)
$(SC.deriveSafeCopy 0 'SC.base ''PasswordBundle)
 
