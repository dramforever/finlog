module Finlog.IR.Node where

import Data.Kind
import Finlog.Framework.DAG
import Finlog.Framework.Graph
import Finlog.Utils.Unique

data Node (m :: Type -> Type)
    = StoreN Reg IName
    | YieldN Unique
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
