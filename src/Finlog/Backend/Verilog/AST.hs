{-# LANGUAGE TemplateHaskell #-}

module Finlog.Backend.Verilog.AST where

import           Data.Derive.TopDown
import           Data.String
import qualified Data.Text as T

newtype VerilogVar = VerilogVar T.Text
    deriving newtype (IsString)

newtype Verilog = Verilog [Module]

data Module = Module VerilogVar [VerilogVar] [Decl]

data Decl
    = DirDecl Dir Typ VerilogVar
    | TypeDecl Net Typ VerilogVar
    | Assign Expr Expr
    | AlwaysFF Edge VerilogVar Stmt

data Stmt
    = Block [Stmt]
    | Expr :=: Expr
    | Expr :<=: Expr
    | If Expr Stmt (Maybe Stmt)
    | Case Expr [(Literal, Stmt)]
    | Comment T.Text

data Expr
    = VarE VerilogVar
    | LitE Literal
    | BinE BinOp Expr Expr
    | CondE Expr Expr Expr

data Literal = Literal Integer Typ


data Net = Wire | Reg
data Dir = Input | Output
data Edge = Posedge | Negedge
data BinOp = Add

data Typ = Bit | Unsigned Int | Signed Int

$(derivings [''Show, ''Eq] ''Verilog)
