module Happstack.Identity.Auth.Password.Types
( HasPasswordManager(..)
) where

------------------------------------------------------------------------------
import qualified Data.Text       as TS
import qualified Data.ByteString as BS
------------------------------------------------------------------------------
import qualified Happstack.Identity.Types as I
------------------------------------------------------------------------------

class HasIdentityManager m => HasPasswordManager m where
    getPasswordHash :: IdentityID m -> m (Maybe I.PasswordHash)
    setPasswordHash :: IdentityID m -> m ()
 
