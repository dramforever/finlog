{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Finlog.Frontend.AST where

import Data.Derive.TopDown
import Data.Text (Text)

data Program = Program [Process]

data Process = Process Var Block

data Stmt
    = Block Block
    | Declare Var Typ Expr
    | Assign Var Expr
    | Yield

type Block = [Stmt]

data Expr
    = VarE Var
    | LitE Literal
    | BinE BinOp Expr Expr

data Literal = IntLitL IntLit

data BinOp = Add

data IntType = Bit | Signed Int | Unsigned Int
data IntLit = IntLit Integer IntType

data Typ = IntType IntType

newtype Var = Var Text

$(derivings [''Show] ''Program)
