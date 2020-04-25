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

badUnaryOp :: BinOp -> Typ -> CompilerError
badUnaryOp op rhs =
    CompilerError
    ("Bad unary operation:"
        <+> codeAnn (viaShow op <+> viaShow rhs))
    []

badBinOp :: BinOp -> Typ -> Typ -> CompilerError
badBinOp op lhs rhs =
    CompilerError
    ("Bad binary operation:"
        <+> codeAnn (viaShow lhs <+> viaShow op <+> viaShow rhs))
    []

condDiffer :: Typ -> Typ -> CompilerError
condDiffer t1 t2 =
    CompilerError
    ("Differing types in conditional" <+> codeAnn (viaShow t1)
        <+> "and" <+> codeAnn (viaShow t2))
    []

-- TODO: Simplify this
combineInt :: Typ -> Typ -> Maybe IntType
combineInt (UnsignedTy s1) (UnsignedTy s2) = Just $ Unsigned (s1 `max` s2)
combineInt (UnsignedTy s1) (SignedTy s2) = Just $ Unsigned (s1 `max` s2)
combineInt (SignedTy s1) (UnsignedTy s2) = Just $ Unsigned (s1 `max` s2)
combineInt (SignedTy s1) (SignedTy s2) = Just $ Signed (s1 `max` s2)
combineInt _ _ = Nothing

inferBin :: BinOp -> Typ -> Typ -> Maybe Typ
inferBin BinArith lhs rhs = IntType <$> combineInt lhs rhs
inferBin BinBitComb BitTy BitTy = Just BitTy
inferBin BinBitComb lhs rhs = IntType <$> combineInt lhs rhs
inferBin BinComp lhs rhs
    | lhs == rhs = Just BitTy
    | otherwise = Nothing
inferBin BinLogic (IntType _) (IntType _) = Just BitTy
inferBin BinLogic _ _  = Nothing


inferExprF :: ExprF Typ -> Either CompilerError Typ
inferExprF (LitE lit) = Right $ literalType lit
inferExprF (InputE _ typ) = Right typ
inferExprF (UnaryE BNot rhs@(IntType _)) = Right rhs
-- inferExprF (UnaryE op@BNot rhs) = Left $ badUnaryOp op rhs
inferExprF (UnaryE LNot (IntType _)) = Right BitTy
-- inferExprF (UnaryE op@LNot rhs) = Left $ badUnaryOp op rhs
inferExprF (BinE op lhs rhs) =
    case inferBin op lhs rhs of
        Just ty -> Right ty
        Nothing -> Left $ badBinOp op lhs rhs
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
