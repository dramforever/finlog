{-# LANGUAGE TemplateHaskell #-}
module Finlog.Framework.Topo where

import           Control.Monad
import           Control.Monad.State
import           Data.Foldable
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Data.Traversable
import           Finlog.Framework.Graph
import           Finlog.Utils.Mark
import           Finlog.Utils.MiniState
import           Finlog.Utils.Queue
import           Lens.Micro.Platform

data TopoPass m a
    = TopoPass
    { _tpStart :: HM.HashMap Label a
    , _tpSucc :: Label -> m [Label]
    , _tpMerge :: a -> a -> m a
    , _tpWork :: Label -> a -> m (HM.HashMap Label a)
    }

data TopoState a
    = TopoState
    { _tsInDegrees :: HM.HashMap Label Int
    , _tsValue :: HM.HashMap Label a
    }
    deriving (Show)

$(makeLenses ''TopoPass)
$(makeLenses ''TopoState)

inDegree :: Label -> Lens' (TopoState m) Int
inDegree lbl = tsInDegrees . at lbl . non 0

bfs :: _ => TopoPass m a -> MiniStateT (TopoState a) m [Label]
bfs pass = do
    go (listQueue . HM.keys $ pass ^. tpStart)
    MiniStateT $
        filterM (\lbl -> (== 0) <$> use (inDegree lbl))
        (HM.keys $ pass ^. tpStart)
    where
        go q = case pop q of
            Nothing -> pure ()
            Just (l, q') -> do
                lsucc <- lift $ (pass ^. tpSucc) l
                new <- fmap catMaybes (lsucc `for` \ls -> do
                    old <- MiniStateT $ inDegree ls <<%= (+1)
                    pure $ if old == 0 then Just ls else Nothing)
                go (pushList new q')

topo :: _ => TopoPass m a -> m (HM.HashMap Label a)
topo pass =
    evalStateT
        (runMiniStateT start)
        (TopoState HM.empty (pass ^. tpStart))
    where
        start = do
            bfs pass >>= loop . listQueue
            res <- MiniStateT $ use tsValue
            remain <- MiniStateT $ use tsInDegrees
            unless (HM.null remain)
                (lift . compilerError $ "Cycle:" <+> viaShow (HM.keys remain))
            pure res
        loop q = case pop q of
            Nothing -> pure ()
            Just (l, q') -> do
                vMay <- MiniStateT $ use $ tsValue . at l
                vMay `for_` \v -> do
                    gen <- lift $ (pass ^. tpWork) l v
                    HM.toList gen `for_` \(lbl, val) ->
                        MiniStateT (use $ tsValue . at lbl) >>= \case
                            Nothing -> MiniStateT $ tsValue . at lbl ?= val
                            Just orig ->
                                lift ((pass ^. tpMerge) orig val)
                                >>= MiniStateT . (tsValue . at lbl ?=)
                lsucc <- lift $ (pass ^. tpSucc) l
                new <- fmap catMaybes $ lsucc `for` \lbl -> do
                    deg <- MiniStateT $ inDegree lbl <%= (subtract 1)
                    pure $ if deg == 0 then Just lbl else Nothing
                loop (pushList new q')
