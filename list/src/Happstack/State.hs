{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Happstack.State
( HasAcidState(..)
, query
, update
, withLocalState
) where

------------------------------------------------------------------------------
import           Prelude
------------------------------------------------------------------------------
import           Control.Exception.Lifted    (bracket)
import           Control.Monad.Trans         (MonadTrans(..), MonadIO(..))
import           Control.Monad.Trans.Control (MonadBaseControl(..))
------------------------------------------------------------------------------
import           Data.Data
import qualified Data.Acid          as AS
import qualified Data.Acid.Local    as AS
import qualified Data.Acid.Advanced as AS
------------------------------------------------------------------------------

class HasAcidState m st where
   getAcidState :: m (AS.AcidState st)

instance (Monad m, HasAcidState m st, MonadTrans mt) => HasAcidState (mt m) st where
   getAcidState = lift getAcidState

-- |
query :: forall event m.
         ( Functor m
         , MonadIO m
         , AS.QueryEvent event
         , HasAcidState m (AS.EventState event)
         )
      => event
      -> m (AS.EventResult event)
query event = do
    st <- getAcidState
    AS.query' (st :: AS.AcidState (AS.EventState event)) event

-- |
update :: forall event m .
          ( Functor m
          , MonadIO m
          , AS.UpdateEvent event
          , HasAcidState m (AS.EventState event)
          )
       => event
       -> m (AS.EventResult event)
update event = do
    st <- getAcidState
    AS.update' (st :: AS.AcidState (AS.EventState event)) event

-- |
withLocalState :: (MonadBaseControl IO m, MonadIO m, AS.IsAcidic st, Typeable st)
               => Maybe FilePath           -- ^ path to state directory
               -> st                       -- ^ initial state value
               -> (AS.AcidState st -> m a) -- ^ function which uses the `AcidState` handle
               -> m a
withLocalState mPath initialState =
    bracket (liftIO $ maybe AS.openLocalState AS.openLocalStateFrom mPath initialState)
            (liftIO . AS.createCheckpointAndClose)
            
