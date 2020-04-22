{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Finlog.Frontend.AST where

import Control.Monad.Free
import Data.Derive.TopDown
import Data.Eq.Deriving
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Text.Megaparsec (SourcePos)
import Text.Show.Deriving

data Program = Program [Process]

data Process = Process Var StmtBlock

data StmtRaw
    = BlockS StmtBlock
    | DeclareS Var Typ Expr
    | AssignS Var Expr
    | YieldS
    | LoopS StmtBlock
    | WhileS Expr StmtBlock
    | IfS Expr StmtBlock (Maybe StmtBlock)

data Stmt = Stmt SourcePos StmtRaw

type StmtBlock = [Stmt]

data ExprF k
    = LitE Literal
    | BinE BinOp k k
    | CondE k k k
    deriving stock (Functor, Foldable, Traversable)

type Expr = Free ExprF Var

data Literal = IntLitL IntLit

data BinOp = Add

data IntType = Bit | Signed Int | Unsigned Int
data IntLit = IntLit Integer IntType

data Typ = IntType IntType

newtype Var = Var Text
    deriving (Eq, Ord)
    deriving newtype Hashable

$(deriveShow1 ''ExprF)
$(deriveEq1 ''ExprF)
$(derivings [''Show, ''Eq] ''Program)
$(derivings [''Generic] ''ExprF)
$(instances [''Hashable] ''ExprF)

varE :: Var -> Expr
varE = Pure

binE :: BinOp -> Expr -> Expr -> Expr
binE op lhs rhs = Free (BinE op lhs rhs)

litE :: Literal -> Expr
litE lit = Free (LitE lit)
