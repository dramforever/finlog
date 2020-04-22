module Finlog.IR.Analysis.Liveness where

import           Control.Monad.State
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.Kind
import           Finlog.Framework.Analysis
import           Finlog.Framework.DAG
import           Finlog.Framework.Graph
import           Finlog.Frontend.AST
import           Finlog.IR.Node

newtype Liveness (m :: Type -> Type)
    = Liveness (HS.HashSet Reg)
    deriving (Show, Eq)
    deriving newtype (Semigroup, Monoid)

instance Applicative m => FactLattice m (Liveness m) where
    bottom = Liveness HS.empty
    Liveness li1 `joinWith` Liveness li2
        | HS.null (li2 `HS.difference` li1) = pure Nothing
        | otherwise = pure $ Just (Liveness $ li1 `HS.union` li2)

instance (HasItemMap s ExprF, MonadState s m)
    => BackFact m (Node m) (Liveness m) where
    Liveness li `backNode` node = case node of
        StoreN oreg iname -> do
            iregs <- exprRegs iname
            pure $ Liveness (HS.union iregs . HS.delete oreg $ li)
        YieldN _ -> pure $ Liveness li
    outMap `backFinal` fin = case fin of
        JumpN lbl -> pure $ outMap HM.! lbl
        CondN iname tlbl elbl -> do
            regs <- exprRegs iname
            pure $
                (outMap HM.! tlbl)
                <> (outMap HM.! elbl)
                <> Liveness regs
        UndefinedN -> pure $ mempty

livenessAnalysis :: _ => Graph (Node m) 'C 'C -> m (FactMap (Liveness m))
livenessAnalysis = backwardAnalysis
