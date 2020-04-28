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
import           Finlog.Utils.Unique
import           GHC.Stack
import           Lens.Micro.Platform

type RegState = HM.HashMap Reg IName

data SymState
    = SymState
    { _ssCond :: IName
    , _ssRegs :: RegState
    }
    deriving (Eq)

$(makeLenses ''SymState)

instance Show SymState where
    showsPrec p (SymState cond regs) = showParen (p > 10) $
        showString "SymState "
        . showsPrec 11 cond
        . showString " "
        . showsPrec 11 (HM.toList regs)

mkBase :: _ => [RegState] -> m RegState
mkBase ms = do
    let keys = HS.toList $ HS.fromList (ms >>= HM.keys)
    HM.fromList <$>
        traverse (\reg -> (reg,) <$> recordReg reg) keys

mergeSymState :: _ => SymState -> SymState -> m SymState
mergeSymState ss1 ss2 = do
    newCond <- recordItem . ComplexItem $
        BinE LOr (ss1 ^. ssCond) (ss2 ^. ssCond)
    base <- mkBase [ss1 ^. ssRegs, ss2 ^. ssRegs]
    let ss1' = (ss1 ^. ssRegs) <> base
        ss2' = (ss2 ^. ssRegs) <> base
        cond = ss1 ^. ssCond
        go m n = recordItem . ComplexItem $ CondE cond m n
    newRegs <- traverse (uncurry go) $ HM.intersectionWith (,) ss1' ss2'
    pure $ SymState newCond newRegs

mergeSymStates :: (HasCallStack, _) => [SymState] -> m SymState
mergeSymStates [] = error "mergeSymStates: empty list"
mergeSymStates (ss : rest) = foldlM mergeSymState ss rest

type SymMap = HM.HashMap YieldId SymState

singleSymMap :: _ => YieldId -> m SymMap
singleSymMap yid = do
    trueName <- recordPartial $ Free (LitE . IntLitL $ IntLit 1 Bit)
    let ss = SymState
            { _ssCond = trueName
            , _ssRegs = HM.empty
            }
    pure $ HM.singleton yid ss

mergeSymMaps :: _ => [SymMap] -> m SymMap
mergeSymMaps =
    traverse mergeSymStates
        . foldl' (HM.unionWith (++)) HM.empty
        . (fmap . fmap) (: [])

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
                let mkSide bl = sym `for` \ss -> do
                        sname <- substitute (ss ^. ssRegs) iname
                        cname <- if bl
                            then pure sname
                            else recordItem . ComplexItem $
                                UnaryE LNot sname
                        cond' <- recordItem . ComplexItem $
                            BinE LAnd (ss ^. ssCond) cname
                        pure $ ss & ssCond .~ cond'
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
symbolicAnalysis build = do
    let gen (yid, lbl) = (lbl,) <$> singleSymMap yid
        mkStart = fmap HM.fromList . traverse gen . HM.toList
    start <- mkStart $ build ^. pbYieldPostLabels
    let pass = TopoPass
            { _tpStart = fmap (: []) start
            , _tpSucc = pure . succSymMap (build ^. pbGraph)
            , _tpMerge = mergeSymMaps
            , _tpWork = (fmap . fmap . fmap . fmap) (: []) $
                workSymMap (build ^. pbGraph)
            }

    topo pass
