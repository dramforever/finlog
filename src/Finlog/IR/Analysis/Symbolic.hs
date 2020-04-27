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
import           GHC.Stack
import           Lens.Micro.Platform

type Condition = HM.HashMap IName Bool

data CondTree a
    = LeafCT a
    | CondCT IName (CondTree a) (CondTree a)
    deriving (Eq)

genTree' :: [(Condition, a)] -> Maybe (CondTree a)
genTree' [] = Nothing
genTree' [(_, a)] = Just (LeafCT a)
genTree' xs = asum $ tryKey <$> HM.keys notEqual
    where
        common = intersectList (fst <$> xs)
        equal = HM.filter allEqual common
        notEqual = HM.filter (not . allEqual) common
        xs' = xs & traversed . _1 %~ (`HM.difference` equal)
        tryKey key =
            let trues = filter (^?! _1 . at key . _Just) xs'
                falses = filter (not . (^?! _1 . at key . _Just)) xs'
            in CondCT key <$> genTree' trues <*> genTree' falses

genTree :: [(Condition, a)] -> Maybe (Condition, CondTree a)
genTree xs = (equal,) <$> genTree' xs
    where
        common = intersectList (fst <$> xs)
        equal = head <$> HM.filter allEqual common

intersectList :: (HasCallStack, _) => [HM.HashMap k v] -> HM.HashMap k [v]
intersectList [] = error "intersectList: empty list"
intersectList xs =
    foldr1 (HM.intersectionWith (++))
    . (fmap . fmap) (: [])
    $ xs

allEqual :: (HasCallStack, _) => [a] -> Bool
allEqual [] = error "allEqual: empty list"
allEqual (x : xs) = all (== x) xs

insertCond :: IName -> Bool -> Condition -> Maybe Condition
insertCond iname bl cond = case cond ^. at iname of
    Just orig
        | orig == bl -> Just cond
        | otherwise -> Nothing
    Nothing -> Just $ HM.insert iname bl cond

instance Show a => Show (CondTree a) where
    showsPrec p n = case n of
        LeafCT x -> showsPrec 11 x
        CondCT cond ty ey -> showParen (p > 10) $
            showsPrec 11 cond
            . showString " t:" . showsPrec 11 ty
            . showString " f:" . showsPrec 11 ey

type RegState = HM.HashMap Reg IName

data SymState
    = SymState
    { _ssCond :: Condition
    , _ssRegs :: RegState
    }
    deriving (Eq)

$(makeLenses ''SymState)

instance Show SymState where
    showsPrec p (SymState cond regs) = showParen (p > 10) $
        showString "SymState "
        . showsPrec 11 (HM.toList cond)
        . showString " "
        . showsPrec 11 (HM.toList regs)

mkBase :: _ => [RegState] -> m RegState
mkBase ms = do
    let keys = HS.toList $ HS.fromList (ms >>= HM.keys)
    HM.fromList <$>
        traverse (\reg -> (reg,) <$> recordReg reg) keys

mergeSymStates :: _ => [SymState] -> m SymState
mergeSymStates states =
    case genTree states' of
        Nothing -> compilerError $ vsep
            [ "Bad conditions:"
            , indent 4 . vsep $ codeAnn . viaShow <$> states'
            , viaShow states
            ]
        Just (newCond, tree) -> do
            base <- mkBase (snd <$> states')
            let gen r (LeafCT m) = pure $ HM.lookupDefault (base HM.! r) r m
                gen r (CondCT cond t e) = do
                    tname <- gen r t
                    ename <- gen r e
                    if tname == ename
                        then pure tname
                        else recordItem $ ComplexItem (CondE cond tname ename)
            newRegs <- fmap HM.fromList $
                (\r -> (r,) <$> gen r tree) `traverse` HM.keys base
            pure $ SymState newCond newRegs
    where
        states' = convert <$> states
        convert ss = (ss ^. ssCond, ss ^. ssRegs)

type SymMap = HM.HashMap YieldId SymState

singleSymMap :: YieldId -> SymMap
singleSymMap yid = HM.singleton yid ss
    where
        ss = SymState
            { _ssCond = HM.empty
            , _ssRegs = HM.empty
            }

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
            { _tpStart = fmap (:[]) (mkStart $ build ^. pbYieldPostLabels)
            , _tpSucc = pure . succSymMap (build ^. pbGraph)
            , _tpMerge = mergeSymMaps
            , _tpWork =
                (fmap . fmap . fmap . fmap) (: []) $
                    workSymMap (build ^. pbGraph)
            }
