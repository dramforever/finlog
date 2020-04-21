-- {-# LANGUAGE TemplateHaskell #-}

module Finlog.Utils.Unique where

import Control.Monad.State
import Data.Hashable
import Lens.Micro.Platform

newtype Unique = Unique Integer
    deriving (Eq, Ord)
    deriving newtype Hashable

instance Show Unique where
    show (Unique n) = "_" ++ show n

newtype UniqueSupply = UniqueSupply { _supplyValue :: Integer }
    deriving (Show)

-- $(makeClassy ''UniqueSupply)

initialSupply :: UniqueSupply
initialSupply = UniqueSupply 0

freshUnique :: (HasUniqueSupply s, MonadState s m) => m Unique
freshUnique = Unique <$> (supplyValue <<%= (+ 1))

-- Avoid Template Haskell to speed up compilation

class HasUniqueSupply c_alpZ where
  uniqueSupply :: Lens' c_alpZ UniqueSupply
  supplyValue :: Lens' c_alpZ Integer
  {-# INLINE supplyValue #-}
  supplyValue = ((.) uniqueSupply) supplyValue
instance HasUniqueSupply UniqueSupply where
  {-# INLINE supplyValue #-}
  uniqueSupply = id
  supplyValue f_alq0 (UniqueSupply x1_alq1)
    = (fmap (\ y1_alq2 -> UniqueSupply y1_alq2)) (f_alq0 x1_alq1)
