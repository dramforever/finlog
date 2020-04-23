module Finlog.Utils.Pretty where

import           Control.Monad.Free
import           Data.Functor.Classes
import qualified Data.HashMap.Strict as HM
import           Data.Text.Prettyprint.Doc
import           Text.Show

prettyHashMap :: (Show k, Show v) => HM.HashMap k v -> Doc ann
prettyHashMap hm
    | HM.null hm = "<nothing>"
    | otherwise = align . vsep . map pair . HM.toList $ hm
    where
        pair :: (Show k, Show v) => (k, v) -> Doc ann
        pair (k, v) = viaShow k <+> "=>" <+> viaShow v

showCleanFree :: (Show1 f, Show a) => Free f a -> String
showCleanFree val = go val ""
    where
        go (Pure a) = showsPrec 11 a
        go (Free fa) = liftShowsPrec (\p -> showParen (p > 10) . go) (showListWith go) 11 fa
