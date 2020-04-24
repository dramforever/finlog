module Finlog.IR.Node where

import Data.Hashable
import Data.Kind
import Finlog.Framework.DAG
import Finlog.Framework.Graph
import Finlog.Utils.Unique

newtype YieldId = YieldId Unique
    deriving newtype (Eq, Ord, Hashable)

instance Show YieldId where
    show (YieldId uniq) = "[yield]" ++ show uniq

freshYieldId :: _ => m YieldId
freshYieldId = YieldId <$> freshUnique

data Node (m :: Type -> Type)
    = StoreN Reg IName
    | YieldN YieldId
    deriving (Show, Eq)

data FinalNode (m :: Type -> Type)
    = JumpN Label
    | CondN IName Label Label
    | UndefinedN
    deriving (Show, Eq)

instance GraphNode (Node m) where
    type Final (Node m) = FinalNode m

    finalTargets (JumpN lbl) = [lbl]
    finalTargets (CondN _ ltrue lfalse) = [ltrue, lfalse]
    finalTargets UndefinedN = []
