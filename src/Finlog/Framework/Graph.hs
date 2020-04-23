-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
module Finlog.Framework.Graph where

import qualified Data.HashMap.Strict as HM
import           Data.Hashable
import           Data.Kind
import qualified Data.Text as T
import           Finlog.Utils.Unique
import           GHC.Generics
import           GHC.Stack
import           Lens.Micro.Platform

data Label = Label T.Text Unique
    deriving (Eq, Generic)
    deriving anyclass Hashable

instance Ord Label where
    Label name1 uniq1 `compare` Label name2 uniq2 =
        (uniq1 `compare` uniq2) <> (name1 `compare` name2)

instance Show Label where
    show (Label name uniq) = T.unpack name ++ show uniq ++ ":"

freshLabel :: _ => T.Text -> m Label
freshLabel name = Label name <$> freshUnique

class GraphNode node where
    type Final (node :: Type) = (final :: Type) | final -> node

    finalTargets :: Final node -> [Label]

type ShowNode node = (Show node, Show (Final node))

data Block node = Block [node] (Final node)

deriving instance ShowNode node => Show (Block node)

data Half node = Half !Label [node]
    deriving (Show, Eq)

-- Open or closed
data Shape = O | C

data MaybeO (sh :: Shape) (a :: Type) where
    JustO :: a -> MaybeO 'O a
    NothingO :: MaybeO 'C a

deriving instance Show a => Show (MaybeO sh a)
deriving instance Eq a => Eq (MaybeO sh a)
deriving instance Ord a => Ord (MaybeO sh a)

instance Hashable a => Hashable (MaybeO sh a) where
    hashWithSalt s NothingO = s `hashWithSalt` (0 :: Int)
    hashWithSalt s (JustO x) = s `hashWithSalt` (1 :: Int) `hashWithSalt` x

data Residue (node :: Type) (e :: Shape) (x :: Shape) where
    OneRes :: [node] -> Residue node 'O 'O
    TwoRes :: MaybeO e (Block node) -> MaybeO x (Half node) -> Residue node e x

deriving instance ShowNode node => Show (Residue node e x)

connectRes
    :: Residue node e 'O
    -> Residue node 'O x
    -> (Residue node e x, Maybe (Label, Block node))
connectRes (OneRes ns1) (OneRes ns2) =
    (OneRes (ns1 ++ ns2), Nothing)
connectRes (OneRes ns1) (TwoRes (JustO (Block e2ns e2f)) x2) =
    (TwoRes (JustO $ Block (ns1 ++ e2ns) e2f) x2, Nothing)
connectRes (TwoRes e1 (JustO (Half x1lbl x1ns))) (OneRes ns2) =
    (TwoRes e1 (JustO $ Half x1lbl (x1ns ++ ns2)), Nothing)
connectRes (TwoRes e1 (JustO (Half x1lbl x1ns))) (TwoRes (JustO (Block e2ns e2f)) x2) =
    (TwoRes e1 x2, Just (x1lbl, Block (x1ns ++ e2ns) e2f))

spliceRes :: Residue node e 'C -> Residue node 'C x -> Residue node e x
spliceRes (TwoRes e1 NothingO) (TwoRes NothingO x2) = TwoRes e1 x2

type BlockMap node = HM.HashMap Label (Block node)

data Graph (node :: Type) (e :: Shape) (x :: Shape)
    = Graph
    { _blockMap :: BlockMap node
    , _residue :: Residue node e x
    }

-- $(makeLenses ''Graph)

deriving instance ShowNode node => Show (Graph node e x)

(><) :: HasCallStack => Graph node e 'O -> Graph node 'O x -> Graph node e x
Graph map1 res1 >< Graph map2 res2
    | not $ null common = error $ "Duplicate label(s): " ++ show (HM.keys common)
    | Just badLabel <- newLabelCheck =
        error $ "Duplicate label(s): " ++ show [badLabel]
    | otherwise = Graph newMap newRes
    where
        (newRes, newBlock) = connectRes res1 res2
        bothMap = map1 <> map2
        common = map1 `HM.intersection` map2
        (newLabelCheck, newMap) = case newBlock of
            Nothing -> (Nothing, bothMap)
            Just (lbl, blk) ->
                ( if lbl `HM.member` bothMap
                    then Just lbl
                    else Nothing
                , HM.insert lbl blk bothMap
                )

(>|<) :: HasCallStack => Graph node e 'C -> Graph node 'C x -> Graph node e x
Graph map1 res1 >|< Graph map2 res2
    | not $ null common = error $ "Duplicate label(s): " ++ show (HM.keys common)
    | otherwise = Graph (map1 <> map2) (res1 `spliceRes` res2)
    where common = map1 `HM.intersection` map2

emptyGraph :: Graph node 'C 'C
emptyGraph = Graph
    { _blockMap = HM.empty
    , _residue = TwoRes NothingO NothingO
    }

emptyOpen :: Graph node 'O 'O
emptyOpen = emptyGraph & residue .~ OneRes []

catGraphs :: [Graph node 'O 'O] -> Graph node 'O 'O
catGraphs = foldr (><) emptyOpen

unionGraphs :: [Graph node 'C 'C] -> Graph node 'C 'C
unionGraphs = foldr (>|<) emptyGraph

mkNode :: node -> Graph node 'O 'O
mkNode n = emptyGraph & residue .~ OneRes [n]

mkNodes :: [node] -> Graph node 'O 'O
mkNodes ns = emptyGraph & residue .~ OneRes ns

mkLabel :: Label -> Graph node 'C 'O
mkLabel lbl = emptyGraph & residue .~ TwoRes NothingO (JustO (Half lbl []))

mkFinal :: Final node -> Graph node 'O 'C
mkFinal fin = emptyGraph & residue .~ TwoRes (JustO (Block [] fin)) NothingO

-- Avoid Template Haskell to speed up compilation

blockMap ::
  forall node_alwx e_alwy x_alwz.
  Lens' (Graph node_alwx e_alwy x_alwz) (BlockMap node_alwx)
blockMap f_amLU (Graph x1_amLV x2_amLW)
  = (fmap (\ y1_amLX -> (Graph y1_amLX) x2_amLW)) (f_amLU x1_amLV)
{-# INLINE blockMap #-}
residue ::
  forall node_alwx e_alwy x_alwz e_amLS x_amLT.
  Lens (Graph node_alwx e_alwy x_alwz) (Graph node_alwx e_amLS x_amLT) (Residue node_alwx e_alwy x_alwz) (Residue node_alwx e_amLS x_amLT)
residue f_amLY (Graph x1_amLZ x2_amM0)
  = (fmap (\ y1_amM1 -> (Graph x1_amLZ) y1_amM1)) (f_amLY x2_amM0)
{-# INLINE residue #-}
