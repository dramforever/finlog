{-# LANGUAGE TemplateHaskell #-}

module Finlog.Backend.Verilog.AST
    ( module Finlog.Backend.Verilog.AST
    , module Finlog.Frontend.AST
    ) where

import           Data.Derive.TopDown
import           Data.String
import qualified Data.Text as T
import           Finlog.Frontend.AST (UnaryOp(..), BinOp(..))

newtype VerilogVar = VerilogVar T.Text
    deriving newtype (IsString)

newtype Verilog = Verilog [Module]

data Module = Module VerilogVar [VerilogVar] [Decl]

data Decl
    = DirDecl Dir Typ VerilogVar
    | Wire Typ VerilogVar Expr
    | Reg Typ VerilogVar
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
    | UnaryE UnaryOp Expr
    | BinE BinOp Expr Expr
    | CondE Expr Expr Expr

data Literal = Literal Integer Typ

data Dir = Input | Output
data Edge = Posedge | Negedge

data Typ = Bit | Unsigned Int | Signed Int

$(derivings [''Show, ''Eq] ''Verilog)
