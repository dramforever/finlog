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
    | DoWhileS Expr StmtBlock
    | IfS Expr StmtBlock (Maybe StmtBlock)

data Stmt = Stmt SourcePos StmtRaw

type StmtBlock = [Stmt]

data ExprF k
    = InputE Var Typ
    | LitE Literal
    | BinE BinOp k k
    | UnaryE UnaryOp k
    | CondE k k k
    deriving stock (Functor, Foldable, Traversable)

type Expr = Free ExprF Var

data Literal = IntLitL IntLit

data UnaryOp
    = BNot
    | LNot
    deriving stock (Generic)
    deriving anyclass (Hashable)

-- Remember to update the pattern synonyms way below
data BinOp
    = Add | Sub
    | BAnd | BOr
    | Equ | Neq | Lt | Gt | Le | Ge
    | LAnd | LOr
    deriving stock (Generic)
    deriving anyclass (Hashable)

data IntType = Bit | Signed Int | Unsigned Int

data IntLit = IntLit Integer IntType

data Typ = IntType IntType

pattern SignedTy :: Int -> Typ
pattern SignedTy s = IntType (Signed s)

pattern UnsignedTy :: Int -> Typ
pattern UnsignedTy s = IntType (Unsigned s)

pattern BitTy:: Typ
pattern BitTy = IntType Bit

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

instance Show UnaryOp where
    show BNot = "~"
    show LNot = "!"

instance Show BinOp where
    show Add = "+"
    show Sub = "-"
    show BAnd = "&"
    show BOr = "|"
    show Equ = "=="
    show Neq = "!="
    show Lt = "<"
    show Gt = ">"
    show Le = "<="
    show Ge = ">="
    show LAnd = "&&"
    show LOr = "||"

instance Show1 ExprF where
    liftShowsPrec sp _ p exprf = case exprf of
        InputE (Var name) typ -> showParen (p > 10) $
            ss (T.unpack name) . ss ": " . showsPrec 10 typ
        LitE lit -> shows lit
        UnaryE op rhs -> showParen (p > 10) $
            shows op . ss " " . sp 10 rhs
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
    hashWithSalt s (UnaryE op rhs) =
            s `hashWithSalt` (3 :: Int)
            `hashWithSalt` op `hashWithSalt` rhs
    hashWithSalt s (BinE op lhs rhs) =
            s `hashWithSalt` (4 :: Int)
            `hashWithSalt` op `hashWithSalt` lhs `hashWithSalt` rhs
    hashWithSalt s (CondE cond t e) =
            s `hashWithSalt` (5 :: Int)
            `hashWithSalt` cond `hashWithSalt` t `hashWithSalt` e

varE :: Var -> Expr
varE = Pure

litE :: Literal -> Expr
litE lit = Free (LitE lit)

unaryE :: UnaryOp -> Expr -> Expr
unaryE op rhs = Free (UnaryE op rhs)

binE :: BinOp -> Expr -> Expr -> Expr
binE op lhs rhs = Free (BinE op lhs rhs)

condE :: Expr -> Expr -> Expr -> Expr
condE cond t e = Free (CondE cond t e)

pattern BinArith :: BinOp
pattern BinArith <- ((`elem` [Add, Sub]) -> True)

pattern BinBitComb :: BinOp
pattern BinBitComb <- ((`elem` [BAnd, BOr]) -> True)

pattern BinComp :: BinOp
pattern BinComp <- ((`elem` [Equ, Neq, Lt, Gt, Le, Ge]) -> True)

pattern BinLogic :: BinOp
pattern BinLogic <- ((`elem` [LAnd, LOr]) -> True)

{-# COMPLETE BinArith, BinBitComb, BinComp, BinLogic #-}
