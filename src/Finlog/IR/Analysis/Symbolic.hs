{-# LANGUAGE TemplateHaskell #-}
module Finlog.IR.Analysis.Symbolic where

import           Control.Monad.Free
import           Control.Monad.State
import           Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.Maybe
import           Data.Traversable
import           Finlog.Framework.DAG
import           Finlog.Framework.Graph
import           Finlog.Framework.Topo
import           Finlog.Frontend.AST
import           Finlog.IR.Build
import           Finlog.IR.Node
import           Finlog.Utils.Mark
import           Finlog.Utils.MiniState
import           Lens.Micro.Platform

type Condition = HM.HashMap IName Bool

mergeCondition
    :: Condition
    -> Condition
    -> Maybe (IName, Bool, Condition)
mergeCondition cond1 cond2 =
    (\(i, b) -> (i, b, agree)) <$> listToMaybe (toList disagree)
    where
        common = HM.intersectionWith (,) cond1 cond2
        agree = HM.mapMaybe getAgree common
        disagree = HM.mapMaybeWithKey getDisagree common

getAgree :: Eq a => (a, a) -> Maybe a
getAgree (x, y) = if x == y then Just x else Nothing

getDisagree :: Eq a => a -> (Bool, Bool) -> Maybe (a, Bool)
getDisagree k (True, False) = Just $ (k, True)
getDisagree k (False, True) = Just $ (k, False)
getDisagree _ _ = Nothing

insertCond :: IName -> Bool -> Condition -> Maybe Condition
insertCond iname bl cond = case cond ^. at iname of
    Just orig
        | orig == bl -> Just cond
        | otherwise -> Nothing
    Nothing -> Just $ HM.insert iname bl cond

data YieldTree
    = YieldYT YieldId
    | CondYT IName YieldTree YieldTree
    deriving (Eq)

instance Show YieldTree where
    showsPrec p n = case n of
        YieldYT yid -> showsPrec 11 yid
        CondYT cond ty ey -> showParen (p > 10) $
            showsPrec 11 cond
            . showString " t:" . showsPrec 11 ty
            . showString " f:" . showsPrec 11 ey

condYT :: IName -> YieldTree -> YieldTree -> YieldTree
condYT iname a b
    | a == b = a
    | otherwise = CondYT iname a b

type RegState = HM.HashMap Reg IName

data SymState
    = SymState
    { _ssCond :: Condition
    , _ssYield :: YieldTree
    , _ssRegs :: RegState
    }
    deriving (Eq)

$(makeLenses ''SymState)

instance Show SymState where
    showsPrec p (SymState cond yi regs) = showParen (p > 10) $
        showString "SymState "
        . showsPrec 11 (HM.toList cond)
        . showString " "
        . showsPrec 11 yi
        . showString " "
        . showsPrec 11 (HM.toList regs)

mkBase :: _ => [RegState] -> m RegState
mkBase ms = do
    let keys = HS.toList $ HS.fromList (ms >>= HM.keys)
    HM.fromList <$>
        traverse (\reg -> (reg,) <$> recordReg reg) keys

mergeSymState :: _ => SymState -> SymState -> m SymState
mergeSymState (SymState cond1 yi1 regs1) (SymState cond2 yi2 regs2) =
    case mergeCondition cond1 cond2 of
        Nothing -> compilerError $ "Bad conditions" <+> codeAnn (viaShow (cond1, cond2))
        Just (key, firstTrue, newCond) -> do
            let switch :: (a -> a -> b) -> (a -> a -> b)
                switch = if firstTrue then id else flip
                newYi = switch (condYT key) yi1 yi2
                mkCond a b
                    | a == b = pure a
                    | otherwise = recordPartial $
                        Free (switch (CondE (Pure key)) (Pure a) (Pure b))
            base <- mkBase [regs1, regs2]
            newRegs <- traverse (uncurry mkCond) $
                HM.intersectionWith (,) (regs1 <> base) (regs2 <> base)
            pure $ SymState newCond newYi newRegs

type SymMap = HM.HashMap YieldId SymState

mergeSymMap :: _ => SymMap -> SymMap -> m SymMap
mergeSymMap a b = sequence $ HM.unionWith go (pure <$> a) (pure <$> b)
    where
        go x y = join (mergeSymState <$> x <*> y)

singleSymMap :: YieldId -> SymMap
singleSymMap yid = HM.singleton yid (SymState HM.empty (YieldYT yid) HM.empty)

getReg :: _ => RegState -> Reg -> m IName
getReg regs r = case regs ^. at r of
    Nothing -> recordReg r
    Just iname -> pure iname

substitute :: _ => RegState -> IName -> m IName
substitute regs topName =
    evalStateT (runMiniStateT (go topName)) HM.empty
    where
        go name =
            MiniStateT (use $ at name) >>= \case
                Nothing -> do
                    res <- lift (use (fwdMap . at name)) >>= \case
                        Nothing -> error $ "substitute: Invalid name " ++ show name
                        Just (RegItem reg) -> lift $ getReg regs reg
                        Just (ComplexItem fn) ->
                            traverse go fn >>= lift . recordItem . ComplexItem
                    MiniStateT $ at name ?= res
                    pure res
                Just cached -> pure cached

succSymMap :: _ => Graph (Node m) 'C 'C -> Label -> [Label]
succSymMap gr lbl = go (gr ^?! blockMap . at lbl . _Just)
    where
        go (Block [] fin) = finalTargets fin
        go (Block (YieldN _ : _) _) = []
        go (Block (_ : ns) fin) = go (Block ns fin)

workSymMap :: _ => Graph (Node m) 'C 'C -> Label -> SymMap -> m (HM.HashMap Label SymMap)
workSymMap gr lbl = go (gr ^?! blockMap . at lbl . _Just)
    where
        go (Block [] fin) sym = case fin of
            JumpN l -> pure $ HM.singleton l sym
            CondN iname tlbl elbl -> do
                let mkSide bl = HM.mapMaybe id <$> sym `for` \ss -> do
                        sname <- substitute (ss ^. ssRegs) iname
                        pure $ forOf ssCond ss (insertCond sname bl)
                tsym <- mkSide True
                fsym <- mkSide False
                pure $ HM.fromList
                    [ (tlbl, tsym)
                    , (elbl, fsym)
                    ]
            UndefinedN -> pure HM.empty
        go (Block (n : ns) fin) sym = case n of
            StoreN reg iname -> do
                sym1 <- forOf (traversed . ssRegs) sym $ \regs -> do
                    sname <- substitute regs iname
                    pure $ regs & at reg ?~ sname
                go (Block ns fin) sym1
            YieldN _ -> pure HM.empty

symbolicAnalysis :: _ => ProcessBuild m -> m (HM.HashMap Label SymMap)
symbolicAnalysis build = topo pass
    where
        gen (yid, lbl) = (lbl, singleSymMap yid)
        mkStart = HM.fromList . map gen . HM.toList
        pass = TopoPass
            { _tpStart = mkStart $ build ^. pbYieldPostLabels
            , _tpSucc = pure . succSymMap (build ^. pbGraph)
            , _tpMerge = mergeSymMap
            , _tpWork = workSymMap (build ^. pbGraph)
            }
