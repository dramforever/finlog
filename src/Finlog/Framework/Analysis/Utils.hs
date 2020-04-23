module Finlog.Framework.Analysis.Utils where

import qualified Data.HashMap.Strict as HM
import           Data.Hashable
import           Debug.Trace
import           Finlog.Framework.Analysis

joinHashMap :: _
    => JoinFunction m l
    -> JoinFunction m (HM.HashMap k l)
joinHashMap jf map1 map2 = do
    let new = map2 `HM.difference` map1
    joined <- sequence $ HM.intersectionWith jf map1 map2
    let joined' = HM.mapMaybe id joined
    pure $ if HM.null joined' && HM.null new
        then Nothing
        else Just $ HM.unions [joined', map1, new]

newtype WithTop a = WithTop { getWithTop :: Maybe a }
    deriving stock (Eq, Foldable, Traversable)
    deriving newtype (Functor, Applicative, Monad, Hashable)

instance Show a => Show (WithTop a) where
    showsPrec _ Top = showString "Top"
    showsPrec p (Under a) = showParen (p > 10) $
        showString "Under " . showsPrec 11 a

{-# COMPLETE Top, Under #-}

pattern Top :: WithTop a
pattern Top = WithTop Nothing

pattern Under :: a -> WithTop a
pattern Under a = WithTop (Just a)

joinTop :: _
    => (l -> l -> m (Maybe (WithTop l)))
    -> JoinFunction m (WithTop l)
joinTop _ Top _ = pure Nothing
joinTop _ _ Top = pure (Just Top)
joinTop jf (Under a) (Under b) = jf a b
