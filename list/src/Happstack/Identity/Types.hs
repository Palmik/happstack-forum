{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE StandaloneDeriving         #-} 
{-# LANGUAGE FlexibleInstances          #-}

module Happstack.Identity.Types
( HasIdentity(..)
, HasIdentityManager(..)

  -- * Credentials	
, Credentials(..)

  -- * Credentials : Password
, PasswordHandle(..)

  -- * Session
, Session(..)
, SessionKey(..)
, SessionCookie(..)
) where

------------------------------------------------------------------------------
import           Prelude
------------------------------------------------------------------------------
import           Control.Monad.Trans(MonadTrans(..))
------------------------------------------------------------------------------
import qualified Data.ByteString as BS
import           Data.Data
import qualified Data.SafeCopy   as SC
import qualified Data.Text       as TS
------------------------------------------------------------------------------

class HasIdentity (m :: * -> *) where
    type IdentityID m :: *

instance HasIdentity m => HasIdentity (mt m) where
    type IdentityID (mt m) = IdentityID m

-- | To use the functionality provided by this package, make your application
-- monad an instance of this class.
class HasIdentity m => HasIdentityManager m where
    -- | Return ID of the identity associated with these credentials. Return Nothing
    -- in case these credentials are not associated with any identity.
    --
    -- NOTE: You should not need to worry about session cookies here.
    signinCredentials :: Credentials -> m (Maybe (IdentityID m))

    -- | Create an identity, associate it with the given credentials and return it's ID.
    -- Return Nothing if the given credentials are already associated with some identity.
    --
    -- NOTE: You should not need to worry about session cookies here.
    signupCredentials :: Credentials -> m (Maybe (IdentityID m))

    -- | Creates a session on the server.
    insertSession :: SessionKey -> Session (IdentityID m) -> m ()

    -- | Reads a session from the server.
    lookupSession :: SessionKey -> m (Maybe (Session (IdentityID m)))

    -- | Updates session on the server. Returns True iff session with the given key existed.
    -- updateSession :: SessionKey -> Session (IdentityID m) -> m Bool

    -- | Deletes session from the server. Returns True iff session with the given key existed.
    deleteSession :: SessionKey -> m Bool

{-
instance HasIdentityManager m => HasIdentityManager (WR.RouteT url m) where
    signinCredentials = WR.liftRouteT . signinCredentials
    signupCredentials = WR.liftRouteT . signupCredentials

    insertSession k = WR.liftRouteT . insertSession k
    lookupSession   = WR.liftRouteT . lookupSession
    deleteSession   = WR.liftRouteT . deleteSession 
-}

instance (Monad m, HasIdentityManager m, MonadTrans mt) => HasIdentityManager (mt m) where
    signinCredentials = lift . signinCredentials
    signupCredentials = lift . signupCredentials

    insertSession k = lift . insertSession k
    lookupSession   = lift . lookupSession
    deleteSession   = lift . deleteSession

------------------------------------------------------------------------------
-- | CREDENTIALS

data Credentials = CredentialsPassword PasswordHandle
    deriving (Data, Eq, Ord, Typeable, Show)

-- | Password ID.
newtype PasswordHandle = PasswordHandle TS.Text
  deriving (Data, Eq, Ord, Typeable, Show)

------------------------------------------------------------------------------
-- | SESSION

newtype SessionKey = SessionKey { unSessionKey :: BS.ByteString }
  deriving (Data, Eq, Ord, Typeable)

data Session a = Session
    { sessionIdentityID :: a 
  --  sessionExpiration :: UTCTime
  --  ...
    } deriving (Data, Eq, Ord, Typeable) 

------------------------------------------------------------------------------
-- | SESSION COOKIE

newtype SessionCookie = SessionCookie { unSessionCookie :: SessionKey }
    
$(SC.deriveSafeCopy 0 'SC.base ''PasswordHandle)
$(SC.deriveSafeCopy 0 'SC.base ''Credentials)
$(SC.deriveSafeCopy 0 'SC.base ''SessionKey)
$(SC.deriveSafeCopy 0 'SC.base ''Session)

