{-# LANGUAGE TemplateHaskell #-}
module Finlog.IR.Analysis.Symbolic where

import           Control.Monad.Free
import           Control.Monad.State
import           Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.Kind
import           Data.Maybe
import           Debug.Trace
import           Finlog.Framework.Analysis
import           Finlog.Framework.Analysis.Utils
import           Finlog.Framework.DAG
import           Finlog.Framework.Graph
import           Finlog.Frontend.AST
import           Finlog.IR.Node
import           Finlog.Utils.Unique
import           GHC.Stack
import           Lens.Micro.Platform

type Condition = HM.HashMap IName Bool

mergeCondition
    :: Condition
    -> Condition
    -> Maybe (IName -> IName -> Free ExprF IName, Condition)
mergeCondition cond1 cond2 =
    (, agree) <$> listToMaybe (toList disagree)
    where
        common = HM.intersectionWith (,) cond1 cond2
        agree = HM.mapMaybe getAgree common
        disagree = HM.mapMaybeWithKey getDisagree common

getAgree :: Eq a => (a, a) -> Maybe a
getAgree (x, y) = if x == y then Just x else Nothing

getDisagree :: Eq a => a -> (Bool, Bool) -> Maybe (a -> a -> Free ExprF a)
getDisagree k (True, False) = Just $ mkCondE k
getDisagree k (False, True) = Just $ flip (mkCondE k)
getDisagree _ _ = Nothing

mkCondE :: Eq a => a -> a -> a -> Free ExprF a
mkCondE c t e
    | t == e = Pure t
    | otherwise = Free $ CondE (Pure c) (Pure t) (Pure e)

data Sym
    = Sym
    { _symCondition :: Condition
    , _symMap :: HM.HashMap Reg IName
    }
    deriving (Eq)

instance Show Sym where
    showsPrec p (Sym cond ma) = showParen (p > 10) $
        showString "Sym "
        . showsPrec 11 (HM.toList cond)
        . showString " "
        . showsPrec 11 (HM.toList ma)

$(makeLenses ''Sym)

joinSym :: _ => Sym -> Sym -> m (Maybe (WithTop Sym))
joinSym (Sym cond1 map1) (Sym cond2 map2) = do
    let common = HM.intersectionWith (,) cond1 cond2
        agree = HM.mapMaybe getAgree common
        commonKeys = HS.toList $ HS.fromList (HM.keys map1 ++ HM.keys map2)
    base <- HM.fromList <$> traverse (\reg -> (reg,) <$> recordReg reg) commonKeys
    let map1' = map1 <> base
        map2' = map2 <> base
    if map1' == map2'
        then pure $ if cond1 == agree
            then Nothing
            else Just . Under $ Sym agree map1'
        else case mergeCondition cond1 cond2 of
            Nothing -> pure $ Just Top
            Just (eif, condMerge) -> do
                mapMerge <-
                    traverse recordPartial $ HM.intersectionWith eif map1' map2'
                traceShowM ("joinSym", map1, mapMerge)
                pure (Just . Under $ Sym condMerge mapMerge)

newtype Symbolic (m :: Type -> Type)
    = Symbolic (HM.HashMap YieldId (WithTop Sym))

instance Show (Symbolic m) where
    showsPrec p (Symbolic ma) = showParen (p > 10) $
        showString "Symbolic " . showsPrec 10 (HM.toList ma)

instance (MonadState s m, HasUniqueSupply s, HasItemMap s ExprF)
    => FactLattice m (Symbolic m) where
    bottom = Symbolic HM.empty
    joinWith (Symbolic map1) (Symbolic map2) =
        fmap Symbolic <$> joinHashMap (joinTop joinSym) map1 map2

-- Get around the fundeps of MonadState
newtype MiniStateT s m a = MiniStateT { runMiniStateT :: StateT s m a }
    deriving newtype (Functor, Applicative, Monad, MonadTrans)

substitute :: (HasCallStack, _) => (Reg -> Free f IName) -> IName -> m IName
substitute f topName = evalStateT (runMiniStateT (go topName)) HM.empty
    where
        go name =
            MiniStateT (use $ at name) >>= \case
                Nothing -> do
                    res <- lift (use (fwdMap . at name)) >>= \case
                        Nothing -> error $ "substitute: Invalid name " ++ show name
                        Just (RegItem reg) -> lift $ recordPartial (f reg)
                        Just (ComplexItem fn) ->
                            traverse go fn >>= lift . recordItem . ComplexItem
                    MiniStateT $ at name ?= res
                    pure res
                Just cached -> pure cached

instance (MonadState s m, HasUniqueSupply s, HasItemMap s ExprF)
    => FwdFact m (Node m) (Symbolic m) where
    Symbolic sym `fwdNode` node = Symbolic <$> case node of
        StoreN reg iname ->
            forOf (traversed . traversed . symMap) sym $ \ma -> do
                regName <- recordReg reg
                let getReg r = HM.lookupDefault regName r ma
                subReg <- substitute (Pure . getReg) iname
                pure $ ma & at reg ?~ subReg
        YieldN yid -> do
            let regs = HS.fromList $ (sym ^.. traversed . traversed . symMap) >>= HM.keys
            emptyMap <- HM.traverseWithKey (\reg _ -> recordReg reg) $ HS.toMap regs
            pure $ HM.singleton yid (Under $ Sym HM.empty emptyMap)
    Symbolic sym `fwdFinal` node = case node of
        JumpN lbl -> pure (HM.singleton lbl (Symbolic sym))
        CondN iname tlbl elbl -> pure $ HM.fromList
            [ (tlbl, Symbolic $ HM.mapMaybe (updateSym True) sym)
            , (elbl, Symbolic $ HM.mapMaybe (updateSym False) sym)
            ]
            where
                updateSym bl = (traversed . symCondition) `traverseOf` insCond bl
                insCond :: Bool -> Condition -> Maybe Condition
                insCond bl cond = case cond ^. at iname of
                    Just orig
                        | orig == bl -> Just cond
                        | otherwise -> Nothing
                    Nothing -> Just $ HM.insert iname bl cond
        UndefinedN -> pure HM.empty

symbolicAnalysis :: _ => Graph (Node m) 'C 'C -> m (FactMap (Symbolic m))
symbolicAnalysis = forwardAnalysis
