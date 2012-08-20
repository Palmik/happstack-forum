module Text.Reform.Extra
( check
, checkM
, checkBool
, checkBoolM
, reformURL
) where

------------------------------------------------------------------------------
import           Prelude
------------------------------------------------------------------------------
import qualified System.Random as R
------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans (MonadIO(..))
------------------------------------------------------------------------------
import qualified Happstack.Server as HA
------------------------------------------------------------------------------
import qualified Web.Routes as WR
------------------------------------------------------------------------------
import qualified Text.Reform              as F
import qualified Text.Reform.Blaze.Common as F 
import qualified Text.Reform.Happstack    as F 
import qualified Text.Blaze.Html          as B (Html)
------------------------------------------------------------------------------

-- | Similar to yesod-form's `check` 
check :: Monad m
      => (a -> Either error b)
      -> F.Form m input error view anyProof a
      -> F.Form m input error view () b
check = flip F.transformEither
{-# INLINE check #-}

-- | Similar to yesod-form's `checkM` 
checkM :: Monad m
       => (a -> m (Either error b))
       -> F.Form m input error view anyProof a
       -> F.Form m input error view () b
checkM = flip F.transformEitherM
{-# INLINE checkM #-}

-- | Similar to yesod-form's `checkBool` 
checkBool :: Monad m
          => (a -> Bool)
          -> error
          -> F.Form m input error view anyProof a
          -> F.Form m input error view () a
checkBool chk err form = F.transformEither form $
    \x -> if chk x then Right x else Left err
{-# INLINE checkBool #-}

-- | Similar to yesod-form's `checkBoolM` (that is if it had one). 
checkBoolM :: Monad m
           => (a -> m Bool)
           -> error
           -> F.Form m input error view anyProof a
           -> F.Form m input error view () a
checkBoolM chk err form = F.transformEitherM form $
    \x -> do { r <- chk x; if r then return $ Right x else return $ Left err }
{-# INLINE checkBoolM #-}

reformURL :: (HA.ToMessage b, HA.Happstack m, Alternative m, WR.MonadRoute m)
          => WR.URL m        
          -> (a -> m b)                                  
          -> Maybe ([(F.FormRange, error)] -> B.Html -> m b) 
          -> F.Form m [HA.Input] error B.Html proof a           
          -> m B.Html
reformURL url success failure form = do
    prefix <- show <$> liftIO (R.randomRIO (0, 4294967296 :: Integer))
    route  <- WR.showURL url
    F.reform (F.form route) "prefix_"  success failure form
{-# INLINE reformURL #-}

