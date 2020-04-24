{-# LANGUAGE TemplateHaskell #-}
module Finlog.Frontend.Type where

import           Control.Monad.Except
import           Data.Foldable
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Finlog.Framework.DAG
import           Finlog.Frontend.AST
import           Finlog.Utils.Mark
import           GHC.Stack
import           Lens.Micro.Platform

literalType :: Literal -> Typ
literalType (IntLitL (IntLit _ intTy)) = IntType intTy

badArith, condDiffer :: Typ -> Typ -> CompilerError
badArith t1 t2 =
    CompilerError
    ("Bad arithmetic on" <+> codeAnn (viaShow t1)
        <+> "and" <+> codeAnn (viaShow t2))
    []
condDiffer t1 t2 =
    CompilerError
    ("Differing types in conditional" <+> codeAnn (viaShow t1)
        <+> "and" <+> codeAnn (viaShow t2))
    []

-- TODO: Simplify this
arith :: Typ -> Typ -> Either CompilerError Typ
arith (IntType (Unsigned s1)) (IntType (Unsigned s2)) =
    Right $ IntType (Unsigned $ s1 `max` s2)
arith (IntType (Unsigned s1)) (IntType (Signed s2)) =
    Right $ IntType (Unsigned $ s1 `max` s2)
arith (IntType (Signed s1)) (IntType (Unsigned s2)) =
    Right $ IntType (Unsigned $ s1 `max` s2)
arith (IntType (Signed s1)) (IntType (Signed s2)) =
    Right $ IntType (Signed $ s1 `max` s2)
arith t1 t2 = Left $ badArith t1 t2

inferExprF :: ExprF Typ -> Either CompilerError Typ
inferExprF (LitE lit) = Right $ literalType lit
inferExprF (BinE Add lhs rhs) = arith lhs rhs
inferExprF (CondE _cond lhs rhs) =
    if lhs == rhs
        then Right lhs
        else Left $ condDiffer lhs rhs

-- TODO
assignable :: Typ -> Typ -> Bool
assignable = (==)

data TypeMap = TypeMap
    { _inameTypeMap :: HM.HashMap IName Typ
    , _regTypeMap :: HM.HashMap Reg Typ
    }
    deriving (Show, Eq)

$(makeClassy ''TypeMap)

initialTypeMap :: TypeMap
initialTypeMap = TypeMap HM.empty HM.empty

inameType :: HasTypeMap s => IName -> Lens' s (Maybe Typ)
inameType iname = inameTypeMap . at iname

regType :: HasTypeMap s => Reg -> Lens' s (Maybe Typ)
regType reg = regTypeMap . at reg

data Task = Open IName | Close IName
    deriving (Show)

infer :: (HasCallStack, _) => IName -> m Typ
infer iname = do
    go [Open iname]
    fromJust <$> use (inameType iname)
    where
        go [] = pure ()
        go (Open i: rest) = do
            use (inameType i) >>= \case
                Just _ -> go rest
                Nothing -> fromJust <$> use (fwdMap . at i) >>= \case
                    RegItem reg -> do
                        ty <- fromJust <$> use (regType reg)
                        inameType i ?= ty
                        go rest
                    ComplexItem exprf ->
                        go $ (Open <$> toList exprf) ++ Close i : rest
        go (Close i : rest) = do
            fromJust <$> use (fwdMap . at i) >>= \case
                RegItem _ -> error "infer: Unexpected RegItem"
                ComplexItem exprf -> do
                    ty <- traverse (fmap fromJust . use . inameType) exprf
                    res <- liftEither $ inferExprF ty
                    inameType i ?= res
            go rest
