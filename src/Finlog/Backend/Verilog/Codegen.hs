module Finlog.Backend.Verilog.Codegen where

import           Data.Char
import           Data.List
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc
import           Finlog.Backend.Verilog.AST

genVar :: VerilogVar -> Doc ann
genVar (VerilogVar var) = pretty escape
    where
        escape
            | T.null var = error "Can't handle empty variable name"
            | T.any isSpace var = error "Can't handle variable name with spaces"
            | not (T.all isAscii var) = error "Can't handle variable name with non-ASCII"
            | T.all okChar var && okHead (T.head var) = var
            | otherwise = "\\" <> var <> " "
        okChar x = isAlphaNum x || x == '$' || x == '_'
        okHead x = isAlpha x || x == '_'

gen :: Verilog -> Doc ann
gen (Verilog mods) = vsep . intersperse "" $ genModule <$> mods

genModule :: Module -> Doc ann
genModule (Module name vars body) = vsep
    [ hcat
        [ "module" <+> genVar name <+> "("
        , hsep . punctuate "," $ genVar <$> vars
        , ")" <> ";"
        ]
    , indent 4 . vsep $ genDecl <$> body
    , "endmodule"
    ]

genDecl :: Decl -> Doc ann
genDecl (DirDecl dir typ name) =
    genDir dir <+> genType typ <+> genVar name <> ";"
genDecl (TypeDecl net typ name) =
    genNet net <+> genType typ <+> genVar name <> ";"
genDecl (Assign dst expr) =
    "assign" <+> genExpr dst <+> "=" <+> genExpr expr <> ";"
genDecl (AlwaysFF edge var stmt) = vsep
    [ "always_ff" <+> "@" <> parens (genEdge edge <+> genVar var)
    , indent 4 $ genStmt stmt
    ]

genStmt :: Stmt -> Doc ann
genStmt (Block stmts) = vsep
    ["begin", indent 4 . vsep $ genStmt <$> stmts, "end"]
genStmt (dst :=: expr) =
    genExpr dst <+> "=" <+> genExpr expr <> ";"
genStmt (dst :<=: expr) =
    genExpr dst <+> "<=" <+> genExpr expr <> ";"
genStmt (If cond t Nothing) =
    "if" <+> parens (genExpr cond)
        <+> genStmt t
        <+> "else" <+> "begin" <+> "end" -- Avoid ambiguity
genStmt (If cond t (Just e)) =
    "if" <+> parens (genExpr cond)
        <+> genStmt t
        <+> "else" <+> genStmt e
genStmt (Case scrut brs) = vsep
    [ "case" <+> parens (genExpr scrut)
    , indent 4 . vsep $ genBranch <$> brs
    , "endcase"
    ]
    where
        genBranch (lit, stmt) =
            genLiteral lit <> ":" <+> genStmt stmt
genStmt (Comment cmt) = vsep $ go <$> T.lines cmt
    where go l = "//" <+> pretty l

genExpr :: Expr -> Doc ann
genExpr (VarE var) = genVar var
genExpr (LitE literal) = genLiteral literal
genExpr (BinE op lhs rhs) =
    parens $ genExpr lhs <+> genBinOp op <+> genExpr rhs
genExpr (CondE cond t e) =
    parens $ genExpr cond <+> "?" <+> genExpr t <+> ":" <+> genExpr e

genLiteral :: Literal -> Doc ann
genLiteral (Literal i Bit) =
    case i of
        0 -> "0"
        1 -> "1"
        _ -> error $ "Invalid literal " ++ show i
genLiteral (Literal i0 (Unsigned s))
    | i0 < 0 = "-" <> viaShow s <> "'" <> "d" <> viaShow (- i0)
    | otherwise = viaShow s <> "'" <> "d" <> viaShow i0
genLiteral (Literal i0 (Signed s))
    | i0 < 0 = "-" <> viaShow s <> "'" <> "sd" <> viaShow (- i0)
    | otherwise = viaShow s <> "'" <> "sd" <> viaShow i0

genNet :: Net -> Doc ann
genNet Wire = "wire"
genNet Reg = "reg"

genDir :: Dir -> Doc ann
genDir Input = "input"
genDir Output = "output"

genEdge :: Edge -> Doc ann
genEdge Posedge = "posedge"
genEdge Negedge = "negedge"

genType :: Typ -> Doc ann
genType Bit = "logic"
genType (Unsigned n) =
    "logic"
    <+> "[" <> viaShow (n - 1) <> ":" <> "0" <> "]"
genType (Signed n) =
    "logic" <+> "signed"
    <+> "[" <> viaShow (n - 1) <> ":" <> "0" <> "]"

genBinOp :: BinOp -> Doc ann
genBinOp Add = "+"
