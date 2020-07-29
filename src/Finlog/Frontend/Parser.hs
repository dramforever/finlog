module Finlog.Frontend.Parser where

import           Control.Applicative hiding (many)
import           Control.Monad
import           Control.Monad.Combinators.Expr
import           Data.Char
import           Data.List
import           Data.Ord
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Void (Void)
import           Finlog.Frontend.AST
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

atom :: String -> Parser a -> Parser a
atom lbl = label lbl . lexeme . try

-- FIXME: Maybe replace with better structure
reservedNames :: [Text]
reservedNames =
    [ "proc"
    , "var"
    , "output"
    , "yield"
    , "if"
    , "else"
    , "loop"
    , "while"
    , "do"
    ]

nameWord :: Parser Text
nameWord = T.cons
    <$> satisfy isStart
    <*> takeWhileP Nothing isIdent
    where
        isStart c = isAlpha c && isAscii c || c == '_'
        isIdent c = isAlphaNum c && isAscii c || c == '_'

reserved :: Text -> Parser ()
reserved res | res `notElem` reservedNames =
    error $ "'" ++ T.unpack res ++ "' is not listed as reserved"
reserved res = atom ("'" ++ T.unpack res ++ "'") $
    nameWord >>= guard . (== res)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

intTypeRaw :: Parser IntType
intTypeRaw =
    Bit <$ single 'b'
    <|> Signed <$ single 'i' <*> L.decimal
    <|> Unsigned <$ single 'u' <*> L.decimal

intType :: Parser IntType
intType = atom "integer type" $ intTypeRaw

typ :: Parser Typ
typ = IntType <$> intType

-- No space allowed after minus sign
signed :: Parser Integer
signed =
    L.signed (pure ()) L.decimal
    <|> "0x" *> L.hexadecimal
    <|> "0b" *> L.binary

intLit :: Parser IntLit
intLit = lexeme . try $
    IntLit <$> signed <*> (intTypeRaw <?> "type suffix")

var :: Parser Var
var = atom "variable name" $ do
    name <- nameWord
    when (name `elem` reservedNames)
        (fail $ "'" ++ T.unpack name ++ "' is a reserved word")
    pure $ Var name

literal :: Parser Literal
literal = IntLitL <$> intLit

primary :: Parser Expr
primary =
    varE <$> var
    <|> litE <$> literal
    <|> parens expr

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    (map . map) genUnary unary
    ++ (map . map) genBin binary
    ++ ternary
    where
        unary =
            [ [BNot]
            , [LNot]
            ]
        binary =
            [ [Add, Sub]
            , [BAnd, BOr]
            , [Equ, Neq, Lt, Gt, Le, Ge]
            , [LAnd, LOr]
            ]
        ternary =
            [ [TernR $ (condE <$ symbol ":") <$ symbol "?"]
            ]

        toStrs :: Show a => [[a]] -> Parser T.Text
        toStrs =
            msum
            . map (symbol . T.pack)
            . sortOn (Down . length)
            . map show
            . concat

        strsElem :: Show a => Parser T.Text -> a -> Parser ()
        strsElem strs a = try $ strs >>= guard . okay
            where okay x = x == T.pack (show a)

        unaryStrs = toStrs unary
        binaryStrs = toStrs binary

        genUnary b = Prefix (unaryE b <$ strsElem unaryStrs b)
        genBin b = InfixL (binE b <$ strsElem binaryStrs b)

expr :: Parser Expr
expr = makeExprParser primary operatorTable

stmtRaw :: Parser StmtRaw
stmtRaw =
    DeclareS
        <$ reserved "var" <*> var
        <* symbol ":" <*> typ
        <* symbol "=" <*> expr <* symbol ";"
    <|> OutputS <$ reserved "output" <*> var <* symbol ";"
    <|> YieldS <$ reserved "yield" <* symbol ";"
    <|> LoopS <$ reserved "loop" <*> block
    <|> WhileS <$ reserved "while" <*> parens expr <*> block
    <|> flip DoWhileS
        <$ reserved "do" <*> block
        <* reserved "while" <*> parens expr <* symbol ";"
    <|> IfS
        <$ reserved "if" <*> parens expr <*> block
        <*> optional (reserved "else" *> block)
    <|> AssignS <$> var <* symbol "=" <*> expr <* symbol ";"
    <|> BlockS <$> block

stmt :: Parser Stmt
stmt = Stmt <$> getSourcePos <*> stmtRaw

block :: Parser StmtBlock
block = symbol "{" *> many stmt <* symbol "}"

input :: Parser Input
input = Input <$> var <* symbol ":" <*> typ

inputs :: Parser [Input]
inputs =
    parens (input `sepBy` symbol ",")
    <|> pure []

process :: Parser Process
process = Process <$ reserved "proc" <*> var <*> inputs <*> block

program :: Parser Program
program = Program <$ sc <*> many process <* eof
