-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Finlog.Framework.DAG where

import           Control.Monad.Free
import           Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.Hashable
import qualified Data.Text as T
import           Finlog.Utils.Unique
import           GHC.Generics
import           GHC.Stack
import           Lens.Micro.Platform

data Reg = Reg T.Text Unique
    deriving (Eq, Generic)
    deriving anyclass Hashable

instance Ord Reg where
    Reg name1 uniq1 `compare` Reg name2 uniq2 =
        (uniq1 `compare` uniq2) <> (name1 `compare` name2)

instance Show Reg where
    show (Reg name uniq) = "%" ++ T.unpack name ++ show uniq

freshReg :: _ => T.Text -> m Reg
freshReg name = Reg name <$> freshUnique

data IName = IName T.Text Unique
    deriving (Eq, Generic)
    deriving anyclass Hashable

instance Ord IName where
    IName name1 uniq1 `compare` IName name2 uniq2 =
        (uniq1 `compare` uniq2) <> (name1 `compare` name2)

instance Show IName where
    show (IName name uniq) = "#" ++ T.unpack name ++ show uniq

freshIName :: _ => T.Text -> m IName
freshIName name = IName name <$> freshUnique

data Item f
    = RegItem Reg
    | ComplexItem (f IName)

deriving instance Show (f IName) => Show (Item f)
deriving instance Eq (f IName) => Eq (Item f)

instance Hashable (f IName) => Hashable (Item f) where
    hashWithSalt s (RegItem reg) = s `hashWithSalt` (0 :: Int) `hashWithSalt` reg
    hashWithSalt s (ComplexItem fn) = s `hashWithSalt` (1 :: Int) `hashWithSalt` fn

data ItemMap f
    = ItemMap
    { _fwdMap :: HM.HashMap IName (Item f)
    , _revMap :: HM.HashMap (Item f) IName
    }

deriving instance Show (f IName) => Show (ItemMap f)
deriving instance Eq (f IName) => Eq (ItemMap f)

-- $(makeClassy ''ItemMap)

emptyItemMap :: ItemMap f
emptyItemMap = ItemMap HM.empty HM.empty

recordItem :: _ => Item f -> m IName
recordItem item = use (revMap . at item) >>= \case
    Just name -> pure name
    Nothing -> do
        name <- freshIName ""
        fwdMap . at name ?= item
        revMap . at item ?= name
        pure name

recordPartial :: _ => Free f IName -> m IName
recordPartial (Pure r) = pure r
recordPartial (Free f) = traverse recordPartial f >>= recordItem . ComplexItem

recordReg :: _ => Reg -> m IName
recordReg = recordItem . RegItem

record :: _ => Free f Reg -> m IName
record freg = traverse recordReg freg >>= recordPartial

report :: (HasCallStack, _) => IName -> m (Free f Reg)
report name = use (fwdMap . at name) >>= \case
    Nothing -> error $ "report: Invalid name " ++ show name
    Just (RegItem reg) -> pure $ Pure reg
    Just (ComplexItem fn) -> Free <$> traverse report fn

exprRegs :: _ => IName -> m (HS.HashSet Reg)
exprRegs iname = HS.fromList . toList <$> report iname

-- Avoid Template Haskell to speed up compilation

class HasItemMap c_aqqQ f_apYZ | c_aqqQ -> f_apYZ where
  itemMap :: Lens' c_aqqQ (ItemMap f_apYZ)
  fwdMap :: Lens' c_aqqQ (HM.HashMap IName (Item f_apYZ))
  {-# INLINE fwdMap #-}
  revMap :: Lens' c_aqqQ (HM.HashMap (Item f_apYZ) IName)
  {-# INLINE revMap #-}
  fwdMap = ((.) itemMap) fwdMap
  revMap = ((.) itemMap) revMap
instance HasItemMap (ItemMap f_apYZ) f_apYZ where
  {-# INLINE fwdMap #-}
  {-# INLINE revMap #-}
  itemMap = id
  fwdMap f_aqqR (ItemMap x1_aqqS x2_aqqT)
    = (fmap (\ y1_aqqU -> (ItemMap y1_aqqU) x2_aqqT)) (f_aqqR x1_aqqS)
  revMap f_aqqV (ItemMap x1_aqqW x2_aqqX)
    = (fmap (\ y1_aqqY -> (ItemMap x1_aqqW) y1_aqqY)) (f_aqqV x2_aqqX)
