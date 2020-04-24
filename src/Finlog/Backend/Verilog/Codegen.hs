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
            | T.all isAlphaNum var && isAlpha (T.head var) = var
            | otherwise = "\\" <> var <> " "

gen :: Verilog -> Doc ann
gen (Verilog mods) = vsep . intersperse "" $ genModule <$> mods

genModule :: Module -> Doc ann
genModule (Module name vars body) = vsep
    [ hcat
        [ "module" <+> genVar name <+> "("
        , hsep . punctuate "," $ genVar <$> vars
        , ")" <> ";"
        ]
    , indent 4 . vsep $ genStatement <$> body
    , "endmodule"
    ]

genStatement :: Statement -> Doc ann
genStatement (DirDecl dir name) = genDir dir <+> genVar name <> ";"
genStatement (TypeDecl net typ name) =
    genNet net <+> genType typ <+> genVar name <> ";"
genStatement (Assign dst expr) =
    "assign" <+> genVar dst <+> "=" <+> genExpr expr <> ";"
genStatement (Always trigger dst src) = vsep
    [ "always" <+> genTrigger trigger
    , indent 4 $ genVar dst <+> "<=" <+> genVar src <> ";"
    ]

genExpr :: Expr -> Doc ann
genExpr (LitE literal) = genLiteral literal
genExpr (BinE op lhs rhs) =
    parens $ genVar lhs <+> genBinOp op <+> genVar rhs

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


genTrigger :: Trigger -> Doc ann
genTrigger (Trigger edge var) = "@" <> parens (genEdge edge <+> genVar var)

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
