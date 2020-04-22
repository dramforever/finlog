{-# LANGUAGE TemplateHaskell #-}

module Finlog.Framework.Analysis where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.Kind
import           Data.Maybe
import           Finlog.Framework.Graph
import           Lens.Micro.Platform

class FactLattice m l | l -> m where
    bottom :: l

    -- | @joinWith a b@ adds the fact @b@ to @a@, and returns @Just newFact@ if
    -- something changed, and @Nothing@ if not.
    joinWith :: l -> l -> m (Maybe l)

data NullFact (m :: Type -> Type) node = NullFact

instance Applicative m => FactLattice m (NullFact m node) where
    bottom = NullFact
    NullFact `joinWith` NullFact = pure Nothing

instance (Applicative m, FactLattice m la, FactLattice m lb)
    => FactLattice m (la, lb) where
    bottom = (bottom, bottom)
    (a1, b1) `joinWith` (a2, b2) =
        combine <$> (a1 `joinWith` a2) <*> (b1 `joinWith` b2)
        where
            combine Nothing Nothing = Nothing
            combine ma mb = Just (fromMaybe a1 ma, fromMaybe b1 mb)

data BlockFact l = BlockFact [l] l
    deriving (Show, Eq)

blockFactHead :: BlockFact l -> l
blockFactHead (BlockFact (l : _) _) = l
blockFactHead (BlockFact [] l) = l

type FactMap l = HM.HashMap Label l
type BlockFactMap l = HM.HashMap Label (BlockFact l)

fact :: FactLattice m l => FactMap l -> Label -> l
mapl `fact` lbl = fromMaybe bottom $ lbl `HM.lookup` mapl

class FwdFact m node l | l -> node where
    fwdNode :: l -> node -> m l
    fwdFinal :: l -> Final node -> m (FactMap l)

instance Applicative m => FwdFact m node (NullFact m node) where
    NullFact `fwdNode` _ = pure NullFact
    NullFact `fwdFinal` _ = pure HM.empty

instance
    ( Applicative m
    , FactLattice m la, FwdFact m node la
    , FactLattice m lb, FwdFact m node lb
    ) => FwdFact m node (la, lb) where
    (a, b) `fwdNode` node = (,) <$> (a `fwdNode` node) <*> (b `fwdNode` node)
    (a, b) `fwdFinal` node = combine <$> (a `fwdFinal` node) <*> (b `fwdFinal` node)
        where
            combine mapa mapb =
                HM.unionWith
                    combineVal
                    ((,bottom) <$> mapa)
                    ((bottom,) <$> mapb)
            combineVal (va, _) (_, vb) = (va, vb)

class BackFact m node l where
    backNode :: l -> node -> m l
    backFinal :: FactMap l -> Final node -> m l

instance Applicative m => BackFact m node (NullFact m node) where
    NullFact `backNode` _ = pure NullFact
    _ `backFinal` _ = pure NullFact

instance (Applicative m, BackFact m node la, BackFact m node lb)
    => BackFact m node (la, lb) where
    (a, b) `backNode` node = (,) <$> (a `backNode` node) <*> (b `backNode` node)
    mapab `backFinal` node = (,) <$> (mapa `backFinal` node) <*> (mapb `backFinal` node)
        where
            mapa = fst <$> mapab
            mapb = snd <$> mapab

fwdBlock :: _ => l -> Block node -> m (BlockFact l, FactMap l)
fwdBlock inl (Block bnodes bfinal) = do
    let go ls lcur [] = pure $ BlockFact (reverse ls) lcur
        go ls lcur (node : ns) = do
            ln <- lcur `fwdNode` node
            go (lcur : ls) ln ns
    bfact@(BlockFact _ finalFact) <- go [] inl bnodes
    outf <- finalFact `fwdFinal` bfinal
    pure (bfact, outf)

backBlock :: _ => FactMap l -> Block node -> m (BlockFact l)
backBlock outMap (Block bnodes bfinal) = do
    finalFact <- outMap `backFinal` bfinal
    let go ls _lcur [] = pure ls
        go ls lcur (node : ns) = do
            ln <- lcur `backNode` node
            go (ln : ls) ln ns
    nodesFacts <- go [] finalFact (reverse bnodes)
    pure (BlockFact nodesFacts finalFact)

slice :: _ => HM.HashMap k v -> [k] -> HM.HashMap k v
slice hm keys = HM.filterWithKey (\k _ -> k `HS.member` keysSet) hm
    where keysSet = HS.fromList keys

updateMulti :: _ => FactMap l -> [(Label, l)] -> m (Maybe (FactMap l))
updateMulti = go False
    where
        go False _ [] = pure Nothing
        go True mapl [] = pure $ Just mapl
        go flag mapl ((lbl, l) : ls) =
            let orig = fromMaybe bottom (mapl ^. at lbl)
            in orig `joinWith` l >>= \case
                Just newl -> go (flag || True) (HM.insert lbl newl mapl) ls
                Nothing -> go flag mapl ls

type Analysis m node l = Label -> Block node -> l -> FactMap l -> m (FactMap l)

finalBottomMap :: _ => Block node -> FactMap l
finalBottomMap (Block _ blkf) = HM.fromList $ (, bottom) <$> finalTargets blkf

generalAnalysis :: _ => Analysis m node l -> Graph node 'C 'C -> m (FactMap l)
generalAnalysis analysis graph = go (const bottom <$> (graph ^. blockMap))
    where
        bmap = graph ^. blockMap

        go factMap = do
            let workBlock lbl inf = do
                    let blk@(Block _ blkf) = bmap ^?! at lbl . _Just
                        outs = finalTargets blkf
                        outMap =
                            (factMap `slice` outs)
                            `HM.union` finalBottomMap blk
                    res <- analysis lbl blk inf outMap
                    pure res
            workList <- sequence $ HM.mapWithKey workBlock factMap
            let additions = HM.elems workList >>= HM.toList
            updateMulti factMap additions >>= \case
                Nothing -> pure factMap
                Just newMap -> go newMap

forwardAnalysis :: _ => Graph node 'C 'C -> m (FactMap l)
forwardAnalysis = generalAnalysis
    (\_lbl blk inf _outf -> snd <$> (inf `fwdBlock` blk))

backwardAnalysis :: _ => Graph node 'C 'C -> m (FactMap l)
backwardAnalysis = generalAnalysis
    (\lbl blk _inf outf ->
        HM.singleton lbl . blockFactHead
        <$> (outf `backBlock` blk))
