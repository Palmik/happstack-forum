{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Happstack.Identity.Types
( HasIdentity(..)
, HasIdentityManager(..)

, Credentials(..)
, Purpose(..)

  -- * Password related types and functions.
, PasswordCredentials(..)

, Password
, PasswordHash

, makePassword
, makePasswordHash
, verifyPassword
) where

------------------------------------------------------------------------------
import           Prelude
------------------------------------------------------------------------------
import           Control.Applicative
------------------------------------------------------------------------------
import qualified Data.ByteString as BS
import           Data.Data
import qualified Data.SafeCopy   as SC
import qualified Data.Text       as TS
------------------------------------------------------------------------------
import qualified Crypto.PasswordStore as PS
------------------------------------------------------------------------------

class HasIdentity (m :: * -> *) where
    type Identity   m :: *
    type IdentityID m :: *    

-- | To use the functionality provided by this package, make your application
-- monad an instance of this class.
class HasIdentity m => HasIdentityManager m where
    getIdentityID :: Credentials -> Purpose -> m (Maybe (IdentityID m))

    encodeIdentityID :: IdentityID m -> String
    decodeIdentityID :: String -> Maybe (IdentityID m)

    -- | Creates a session on the server.
    createSession :: I.SessionKey -> I.Session m -> m ()

    -- | Reads a session from the server.
    readSession :: I.SessionKey -> m (Maybe (I.Session m))

    -- | Updates session on the server. Returns True iff session with the given key existed.
    updateSession :: I.SessionKey -> I.Session m -> m Bool

    -- | Deletes session from the server. Returns True iff session with the given key existed.
    deleteSession :: I.SessionKey -> m Bool

------------------------------------------------------------------------------
-- | AUTHENTICATION

data Purpose = PurposeLogin
             | PurposeAssociate

------------------------------------------------------------------------------
-- | CREDENTIALS

data Credentials = PasswordCreds PasswordCredentials deriving (Eq, Ord, Typeable)

------------------------------------------------------------------------------
-- | PASSWORD CREDENTIALS

data PasswordCredentials = PasswordCredentials
    { credentialsUserName     :: TS.Text
    , credentialsPasswordHash :: PasswordHash
    } deriving (Eq, Ord, Typeable)

-----

newtype Password = Password BS.ByteString deriving (Eq, Ord, Typeable)
newtype PasswordHash = PasswordHash BS.ByteString deriving (Eq, Ord, Typeable)

-- | Wraps the bytestring in the `Password` constructor.
makePassword :: BS.ByteString -> Password
makePassword = Password

-- | Equivalent to `Crypto.PasswordStore.makePassword`
makePasswordHash :: Password -> Int -> IO PasswordHash
makePasswordHash (Password pass) str = PasswordHash <$> PS.makePassword pass str

-- | Equivalent to `Crypto.PasswordStore.verifyPassword`
verifyPassword :: Password -> PasswordHash -> Bool
verifyPassword (Password pass) (PasswordHash hash) = PS.verifyPassword pass hash

$(SC.deriveSafeCopy 0 'SC.base ''PasswordHash)
$(SC.deriveSafeCopy 0 'SC.base ''PasswordCredentials)
$(SC.deriveSafeCopy 0 'SC.base ''Credentials)

