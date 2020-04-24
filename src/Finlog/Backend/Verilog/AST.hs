{-# LANGUAGE TemplateHaskell #-}

module Finlog.Backend.Verilog.AST where

import           Data.Derive.TopDown
import           Data.String
import qualified Data.Text as T

newtype VerilogVar = VerilogVar T.Text
    deriving newtype (IsString)

newtype Verilog = Verilog [Module]

data Module = Module VerilogVar [VerilogVar] [Statement]

data Statement
    = DirDecl Dir VerilogVar
    | TypeDecl Net Typ VerilogVar
    | Assign VerilogVar Expr
    | Always Trigger VerilogVar VerilogVar

data Expr
    = LitE Literal
    | BinE BinOp VerilogVar VerilogVar

data Literal = Literal Integer Typ

data Trigger = Trigger Edge VerilogVar

data Net = Wire | Reg
data Dir = Input | Output
data Edge = Posedge | Negedge
data BinOp = Add

data Typ = Bit | Unsigned Int | Signed Int

$(derivings [''Show, ''Eq] ''Verilog)
