{-# LANGUAGE TemplateHaskell #-}
module Finlog.IR.Analysis.Symbolic where

import           Control.Monad.Except
import           Control.Monad.Free
import           Control.Monad.State
import           Data.Foldable
import           Data.Function
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.Kind
import           Data.List
import           Data.Maybe
import           Data.Text.Prettyprint.Doc
import           Debug.Trace
import           Finlog.Framework.Analysis
import           Finlog.Framework.Analysis.Utils
import           Finlog.Framework.DAG
import           Finlog.Framework.Graph
import           Finlog.Frontend.AST
import           Finlog.IR.Node
import           Finlog.Utils.Mark
import           Finlog.Utils.MiniState
import           Finlog.Utils.Pretty
import           Finlog.Utils.Unique
import           GHC.Stack
import           Lens.Micro.Platform
import           Text.Show.Deriving

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

data YieldTree
    = YieldYT YieldId
    | CondYT IName YieldTree YieldTree
    deriving (Show, Eq)

type RegState = HM.HashMap Reg IName

data SymState
    = SymState
    { _ssYield :: YieldTree
    , _ssRegs :: RegState
    }
    deriving (Eq)

instance Show SymState where
    showsPrec p (SymState yi regs) = showParen (p > 10) $
        showString "SymState "
        . showsPrec 10 yi
        . showString " "
        . showsPrec 10 regs

$(makeLenses ''SymState)

type Branches = HM.HashMap Condition SymState

data Sym
    = Sym
    { _symCondition :: Condition
    , _symState :: SymState
    , _symBranches :: Branches
    }
    deriving (Eq)

instance Show Sym where
    showsPrec p (Sym cond ma br) = showParen (p > 10) $
        showString "Sym "
        . showsPrec 10 (sortOn fst $ HM.toList cond)
        . showString " "
        . showsPrec 10 ma
        . showString " "
        . showsPrec 10
            (sortOn fst (HM.toList br)
                & mapped . _1 %~ (sortOn fst . HM.toList))

instance Pretty Sym where
    pretty (Sym cond ma br) =
        vsep
        [ "condition:"
        , indent 4 $ prettyHashMap cond
        , "yield id:" <+> viaShow (ma ^. ssYield)
        , "primary:"
        , indent 4 $ prettyHashMap (ma ^. ssRegs)
        , "branches:"
        , indent 4 $ viaShow br
        ]

$(makeLenses ''Sym)

mkBase :: _ => [RegState] -> m RegState
mkBase ms = do
    let keys = HS.toList $ HS.fromList (ms >>= HM.keys)
    HM.fromList <$>
        traverse (\reg -> (reg,) <$> recordReg reg) keys

regStateEqual :: _ => RegState -> RegState -> m Bool
regStateEqual m1 m2 = do
    base <- mkBase [m1, m2]
    pure $ (m1 <> base) == (m2 <> base)

symStateEqual :: _ => SymState -> SymState -> m Bool
SymState yi1 m1 `symStateEqual` SymState yi2 m2 =
    (yi1 == yi2 &&) <$> regStateEqual m1 m2

joinBranches :: _ => Branches -> Branches -> m (Maybe (WithTop Branches))
joinBranches br1 br2 = do
    let common = HM.intersectionWith (,) br1 br2
        new = br2 `HM.difference` br1
    okay <- and <$> traverse (uncurry symStateEqual) common
    if okay
        then if HM.null new
            then pure Nothing
            else pure $ Just (Under $ br1 <> br2)
        else pure $ Just Top

checkSuper :: _ => Branches -> Branches -> m (Bool, Bool)
checkSuper m1 m2 = do
    let common = HM.intersectionWith (,) m1 m2
        only1 = m1 `HM.difference` m2
        only2 = m2 `HM.difference` m1
    okay <- and <$> traverse (uncurry symStateEqual) common
    pure $
        if okay
        then (HM.null only2, HM.null only1)
        else (False, False)

joinSym :: _ => Sym -> Sym -> m (Maybe (WithTop Sym))
joinSym (Sym cond1 ss1@(SymState yi1 map1) br1) sym2@(Sym cond2 ss2@(SymState yi2 map2) br2) = do
    (super1, super2) <- checkSuper br1 br2
    if super1
        then pure Nothing
        else if super2 then pure $ Just (Under sym2)
        else joinBranches br1 br2 >>= \case
            Nothing -> pure Nothing
            Just Top -> pure $ Just Top
            Just (Under newBr)
                | ss1 == ss2 ->
                    let common = HM.intersectionWith (,) cond1 cond2
                        agree = HM.mapMaybe getAgree common
                    in pure $ Just (Under $ Sym agree ss1 newBr)
                | otherwise ->
                    case mergeCondition cond1 cond2 of
                        Nothing -> do
                            traceShowM ("Bad cond", cond1, cond2)
                            pure (Just Top)
                        Just (key, firstTrue, newCond) -> do
                            let mergef a b
                                    | a == b = Pure a
                                    | firstTrue = Free (CondE (Pure key) (Pure a) (Pure b))
                                    | otherwise = Free (CondE (Pure key) (Pure b) (Pure a))
                            base <- mkBase [map1, map2]
                            let map12 = HM.intersectionWith (,)
                                    (map1 <> base) (map2 <> base)
                                gen (a, b) = recordPartial $ mergef a b
                            newMap <- traverse gen map12
                            let newYi
                                    | yi1 == yi2 = yi1
                                    | firstTrue = CondYT key yi1 yi2
                                    | otherwise = CondYT key yi2 yi1
                            pure $ Just (Under $ Sym newCond (SymState newYi newMap) newBr)

newtype Symbolic (m :: Type -> Type)
    = Symbolic { getSymbolic :: HM.HashMap YieldId (WithTop Sym) }

instance Show (Symbolic m) where
    showsPrec p (Symbolic ma) = showParen (p > 10) $
        showString "Symbolic " . showsPrec 10 (sortOn fst $ HM.toList ma)

instance Pretty (Symbolic m) where
    pretty (Symbolic sym) =
        vsep ["Symbolic", indent 4 . vsep . map go . sortOn fst . HM.toList $ sym]
        where
            go (lbl, Top) = viaShow lbl <+> "=>" <+> "Top"
            go (lbl, Under s) = vsep [viaShow lbl <+> "=>", indent 4 $ pretty s]

instance (MonadState s m, HasUniqueSupply s, HasItemMap s ExprF)
    => FactLattice m (Symbolic m) where
    bottom = Symbolic HM.empty
    joinWith (Symbolic map1) (Symbolic map2) =
        fmap Symbolic <$> joinHashMap (joinTop joinSym) map1 map2

substitute :: (HasCallStack, _) => (Reg -> m IName) -> IName -> m IName
substitute f topName = evalStateT (runMiniStateT (go topName)) HM.empty
    where
        go name =
            MiniStateT (use $ at name) >>= \case
                Nothing -> do
                    res <- lift (use (fwdMap . at name)) >>= \case
                        Nothing -> error $ "substitute: Invalid name " ++ show name
                        Just (RegItem reg) -> lift $ f reg
                        Just (ComplexItem fn) ->
                            traverse go fn >>= lift . recordItem . ComplexItem
                    MiniStateT $ at name ?= res
                    pure res
                Just cached -> pure cached

instance (MonadState s m, HasUniqueSupply s, HasItemMap s ExprF)
    => FwdFact m (Node m) (Symbolic m) where
    Symbolic sym `fwdNode` node = Symbolic <$> case node of
        StoreN reg iname -> do
            let go ma = do
                    let getReg r = case HM.lookup r ma of
                            Just rn -> pure rn
                            Nothing -> recordReg r
                    subReg <- substitute getReg iname
                    pure $ ma & at reg ?~ subReg
            sym1 <- forOf (traversed . traversed . symState . ssRegs) sym go
            forOf (traversed . traversed . symBranches . traversed . ssRegs) sym1 go
        YieldN yid -> do
            let initialState = SymState (YieldYT yid) HM.empty
            pure . HM.singleton yid . Under $
                Sym
                { _symCondition = HM.empty
                , _symState = initialState
                , _symBranches = HM.singleton HM.empty initialState
                }
    Symbolic sym `fwdFinal` node = case node of
        JumpN lbl -> pure (HM.singleton lbl (Symbolic sym))
        CondN iname tlbl elbl -> pure $ HM.fromList
            [ (tlbl, Symbolic $ HM.mapMaybe (updateSym True) sym)
            , (elbl, Symbolic $ HM.mapMaybe (updateSym False) sym)
            ]
            where
                updateSym bl s0 = do
                    s1 <- forOf (traversed . symCondition) s0 (insCond bl)
                    forOf (traversed . symBranches) s1 $ \ma ->
                        let go (cond, val) = (, val) <$> insCond bl cond
                        in HM.fromList <$> traverse go (HM.toList ma)
                insCond :: Bool -> Condition -> Maybe Condition
                insCond bl cond = case cond ^. at iname of
                    Just orig
                        | orig == bl -> Just cond
                        | otherwise -> Nothing
                    Nothing -> Just $ HM.insert iname bl cond
        UndefinedN -> pure HM.empty

symbolicAnalysis :: _ => Graph (Node m) 'C 'C -> m (FactMap (Symbolic m))
symbolicAnalysis = forwardAnalysis
