{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Finlog.Frontend.AST where

import           Control.Monad.Free
import           Data.Derive.TopDown
import           Data.Eq.Deriving
import           Data.Functor.Classes
import           Data.Hashable
import qualified Data.Text as T
import           GHC.Generics
import           Text.Megaparsec (SourcePos)

data Program = Program [Process]

data Process = Process Var [Input] StmtBlock

data Input = Input Var Typ

data StmtRaw
    = BlockS StmtBlock
    | DeclareS Var Typ Expr
    | OutputS Var
    | AssignS Var Expr
    | YieldS
    | LoopS StmtBlock
    | WhileS Expr StmtBlock
    | IfS Expr StmtBlock (Maybe StmtBlock)

data Stmt = Stmt SourcePos StmtRaw

type StmtBlock = [Stmt]

data ExprF k
    = InputE Var Typ
    | LitE Literal
    | BinE BinOp k k
    | CondE k k k
    deriving stock (Functor, Foldable, Traversable)

type Expr = Free ExprF Var

data Literal = IntLitL IntLit

data BinOp = Add
    deriving stock (Generic)
    deriving anyclass (Hashable)

data IntType = Bit | Signed Int | Unsigned Int

data IntLit = IntLit Integer IntType

data Typ = IntType IntType

newtype Var = Var T.Text
    deriving (Eq, Ord)
    deriving newtype Hashable

instance Show IntType where
    show Bit = "b"
    show (Signed s) = "i" ++ show s
    show (Unsigned s) = "u" ++ show s

instance Show IntLit where
    show (IntLit int typ) = show int ++ show typ

instance Show Literal where
    show (IntLitL ilit) = "<" ++ show ilit ++ ">"

instance Show BinOp where
    show Add = "+"

instance Show1 ExprF where
    liftShowsPrec sp _ p exprf = case exprf of
        InputE (Var name) typ -> showParen (p > 10) $
            showString (T.unpack name) . ss ": " . showsPrec 10 typ
        LitE lit -> shows lit
        BinE op lhs rhs -> showParen (p > 10) $
            sp 10 lhs . ss " " . shows op . ss " " . sp 10 rhs
        CondE cond t e -> showParen (p > 10) $
            ss "if " . sp 10 cond
            . ss " then " . sp 10 t
            . ss " else " . sp 10 e
        where
            ss = showString

instance Show a => Show (ExprF a) where
    showsPrec p a = liftShowsPrec showsPrec showList p a


$(deriveEq1 ''ExprF)
$(derivings [''Show, ''Eq] ''Program)

$(derivings [''Generic, ''Hashable] ''Literal)
$(derivings [''Generic, ''Hashable] ''Typ)


instance Hashable a => Hashable (ExprF a) where
    hashWithSalt s (InputE var typ) =
            s `hashWithSalt` (0 :: Int)
            `hashWithSalt` var `hashWithSalt` typ
    hashWithSalt s (LitE lit) =
            s `hashWithSalt` (1 :: Int)
            `hashWithSalt` lit
    hashWithSalt s (BinE op lhs rhs) =
            s `hashWithSalt` (2 :: Int)
            `hashWithSalt` op `hashWithSalt` lhs `hashWithSalt` rhs
    hashWithSalt s (CondE cond t e) =
            s `hashWithSalt` (2 :: Int)
            `hashWithSalt` cond `hashWithSalt` t `hashWithSalt` e

varE :: Var -> Expr
varE = Pure

binE :: BinOp -> Expr -> Expr -> Expr
binE op lhs rhs = Free (BinE op lhs rhs)

litE :: Literal -> Expr
litE lit = Free (LitE lit)
