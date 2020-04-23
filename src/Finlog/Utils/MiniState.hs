module Finlog.Utils.MiniState where

import Control.Monad.State

-- Get around the fundeps of MonadState
newtype MiniStateT s m a = MiniStateT { runMiniStateT :: StateT s m a }
    deriving newtype (Functor, Applicative, Monad, MonadTrans)
