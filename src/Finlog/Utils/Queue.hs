-- | A crappy but (probably) good enough queue

module Finlog.Utils.Queue
    ( Queue
    , emptyQueue
    , listQueue
    , push
    , pushList
    , pop
    ) where

import Data.Traversable
import GHC.Exts

data Queue a = Queue [a] [a]

instance IsList (Queue a) where
    type Item (Queue a) = a

    fromList = listQueue
    toList = toList -- From Foldable

instance Show a => Show (Queue a) where
    show q = "fromList " ++ show (toList q)

instance Traversable Queue where
    traverse f (Queue outs ins) =
        Queue
        <$> traverse f outs
        <*> (reverse <$> traverse f (reverse ins))

instance Foldable Queue where foldMap = foldMapDefault
instance Functor Queue where fmap = fmapDefault

emptyQueue :: Queue a
emptyQueue = Queue [] []

listQueue :: [a] -> Queue a
listQueue xs = Queue xs []

push :: a -> Queue a -> Queue a
push a (Queue outs ins) = Queue outs (a : ins)

pushList :: [a] -> Queue a -> Queue a
pushList as (Queue outs ins) = Queue outs (as ++ ins)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue [] ins) = pop (Queue (reverse ins) [])
pop (Queue (x : xs) ins) = Just (x, Queue xs ins)
