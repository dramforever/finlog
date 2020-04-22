{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

module Finlog.Play.Analysis where

import           Control.Monad.Free
import           Control.Monad.State
import           Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.Hashable
import           Data.Kind
import           Data.List
import           Finlog.Framework.Analysis
import           Finlog.Framework.DAG
import           Finlog.Framework.Graph
import           Finlog.Utils.Unique
import           GHC.Generics
import           Lens.Micro.Platform

newtype Liveness (m :: Type -> Type) = Liveness (HS.HashSet Reg)
    deriving (Show, Eq)
    deriving newtype (Semigroup, Monoid)

instance Applicative m => FactLattice m (Liveness m) where
    bottom = Liveness HS.empty
    Liveness li1 `joinWith` Liveness li2
        | HS.null (li2 `HS.difference` li1) = pure Nothing
        | otherwise = pure $ Just (Liveness $ li1 `HS.union` li2)

data Node (m :: Type -> Type)
    = Assign Reg IName
    | Print IName
    deriving (Show, Eq)

data NodeFinal (m :: Type -> Type)
    = Cond IName Label Label
    | Jump Label
    | Halt
    deriving (Show, Eq)

instance GraphNode (Node m) where
    type Final (Node m) = NodeFinal m

    finalTargets (Cond _ l1 l2) = [l1, l2]
    finalTargets (Jump l) = [l]
    finalTargets Halt = []

exprRegs :: _ => IName -> m (HS.HashSet Reg)
exprRegs iname = HS.fromList . toList <$> report iname

instance (HasItemMap s f, MonadState s m, Traversable f)
    => BackFact m (Node m) (Liveness m) where
    backNode (Liveness li) node = case node of
        Assign reg iname -> do
            regs <- exprRegs iname
            pure . Liveness . HS.union regs . HS.delete reg $ li
        Print iname -> do
            regs <- exprRegs iname
            pure . Liveness . HS.union regs $ li
    backFinal outMap fin = case fin of
        Cond cond l1 l2 -> do
            regs <- exprRegs cond
            pure $ (outMap HM.! l1) <> (outMap HM.! l2) <> Liveness regs
        Jump l -> pure $ outMap HM.! l
        Halt -> pure $ Liveness HS.empty

data ExprF k
    = Eq k k
    | Lt k k
    | Plus k k
    | Minus k k
    | Const Integer
    deriving (Show, Eq, Generic, Functor, Foldable, Traversable)
    deriving anyclass Hashable

buildGraph :: forall m. _ => m _
buildGraph = do
    c0 <- freshLabel "c0"
    c1 <- freshLabel "c1"
    c2 <- freshLabel "c2"
    c3 <- freshLabel "c3"
    c4 <- freshLabel "c4"
    c5 <- freshLabel "c5"
    c6 <- freshLabel "c6"

    a <- freshReg "a"
    b <- freshReg "b"
    i <- freshReg "i"
    x <- freshReg "x"
    t <- freshReg "t"
    p <- freshReg "p"

    zero <- record $ Free (Const 0)
    ep <- record $ Pure p
    iltp <- record $ Free (Lt (Pure i) (Pure p))
    bp1 <- record $ Free (Plus (Pure b) (Free $ Const 1))
    ap1 <- record $ Free (Plus (Pure a) (Free $ Const 1))
    aeqb <- record $ Free (Eq (Pure a) (Pure b))
    et <- record $ Pure t
    xp1 <- record $ Free (Plus (Pure x) (Free $ Const 1))
    xm1 <- record $ Free (Minus (Pure x) (Free $ Const 1))
    ip1 <- record $ Free (Plus (Pure i) (Free $ Const 1))

    let z0 = mkLabel c0 >< mkNodes [Assign i zero, Assign b zero, Assign x ep] >< mkFinal (Jump c1)
        z1 = mkLabel c1 >< mkFinal (Cond iltp c2 c3)
        z2 = mkLabel c2 >< mkNodes [Assign b bp1, Assign x zero] >< mkFinal Halt
        z3 = mkLabel c3 >< mkNodes [Assign t ap1, Assign b et] >< mkFinal (Cond aeqb c4 c5)
        z4 = mkLabel c4 >< mkNodes [Assign a xp1] >< mkFinal (Jump c6)
        z5 = mkLabel c5 >< mkNodes [Assign a xm1] >< mkFinal (Jump c6)
        z6 = mkLabel c6 >< mkNodes [Assign i ip1] >< mkFinal (Jump c3)

    pure (z0 >|< z1 >|< z2 >|< z3 >|< z4 >|< z5 >|< z6 :: Graph (Node m) 'C 'C)
    -- let blk :: Block (Node m)
    --     blk = Block [Assign t ap1, Assign b et] (Cond aeqb c4 c5)
    -- bfact <- (finalBottomMap blk :: FactMap (Liveness m)) `backBlock` blk
    -- pure (blk, bfact)

data ProgState f
    = ProgState
    { _psUniqueSupply :: UniqueSupply
    , _psItemMap :: ItemMap f
    }

deriving instance Show (f IName) => Show (ProgState f)

$(makeLenses ''ProgState)

instance HasUniqueSupply (ProgState f) where uniqueSupply = psUniqueSupply
instance HasItemMap (ProgState f) f where itemMap = psItemMap

initialProgState :: ProgState f
initialProgState = ProgState initialSupply emptyItemMap

run :: State (ProgState f) a -> a
run act = evalState act initialProgState

runT :: Monad m => StateT (ProgState f) m a -> m a
runT act = evalStateT act initialProgState

printMap :: _ => HM.HashMap k v -> IO ()
printMap = mapM_ print . sortOn fst . HM.toList

analysis :: forall m. _ => m ()
analysis = do
    gr <- buildGraph
    liftIO . putStrLn $ "=== CFG ==="
    liftIO . printMap $ gr ^. blockMap
    liftIO . putStrLn $ "=== DAG ==="
    use fwdMap >>= liftIO . printMap
    (liveness :: FactMap (Liveness m)) <- backwardAnalysis gr
    liftIO . putStrLn $ "=== Liveness ==="
    liftIO . printMap $ liveness
