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
, PasswordHandle(..)
, makePassword
, makePasswordSaltedHash
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
import qualified Data.Text       as TS
import qualified Data.ByteString as BS
import           Data.IxSet            (Indexable(..), ixSet, ixFun)
------------------------------------------------------------------------------
import qualified Crypto.PasswordStore as PS
------------------------------------------------------------------------------
import qualified Happstack.Identity.Types as I
------------------------------------------------------------------------------

class I.HasIdentityManager m => HasPasswordManager m where
    insertPasswordBundle :: PasswordBundle -> m (Maybe I.PasswordID) 
    lookupPassword :: PasswordHandle -> m (Maybe (I.PasswordID, PasswordSaltedHash))     

instance (Monad m, HasPasswordManager m, MonadTrans mt) => HasPasswordManager (mt m) where
    insertPasswordBundle = lift . insertPasswordBundle
    lookupPassword = lift . lookupPassword

data PasswordBundle = PasswordBundle
    { passwordHandle     :: PasswordHandle
    , passwordSaltedHash :: PasswordSaltedHash
    } deriving (Eq, Ord, Typeable)

instance Indexable (I.PasswordID, PasswordBundle) where
    empty = ixSet
      [ ixFun $ \e -> [ fst e ]
      , ixFun $ \e -> [ passwordHandle $ snd e ]
      ]

------------------------------------------------------------------------------
-- | CONVENIENCE FUNCTIONS AND TYPES (simple wrappers)

newtype PasswordHandle = PasswordHandle TS.Text
  deriving (Eq, Ord, Typeable)
  
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
$(SC.deriveSafeCopy 0 'SC.base ''PasswordHandle)
$(SC.deriveSafeCopy 0 'SC.base ''PasswordBundle)
 
