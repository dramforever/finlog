module Finlog.Frontend.Parser where

import           Control.Applicative hiding (many)
import           Control.Monad
import           Control.Monad.Combinators.Expr
import           Data.Char
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
    , "yield"
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
    error $ T.unpack res ++ " is not listed as reserved"
reserved res = atom ("'" ++ T.unpack res ++ "'") $
    nameWord >>= guard . (== res)

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
signed = L.signed (pure ()) L.decimal

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
    VarE <$> var
    <|> LitE <$> literal
    <|> between (symbol "(") (symbol ")") expr

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [ InfixL (BinE Add <$ symbol "+") ]
    ]

expr :: Parser Expr
expr = makeExprParser primary operatorTable

stmt :: Parser Stmt
stmt =
    Declare
        <$ reserved "var" <*> var
        <* symbol ":" <*> typ
        <* symbol "=" <*> expr <* symbol ";"
    <|> Assign <$> var <* symbol "=" <*> expr <* symbol ";"
    <|> Yield <$ reserved "yield" <* symbol ";"
    <|> Block <$> block

block :: Parser [Stmt]
block = symbol "{" *> many stmt <* symbol "}"

process :: Parser Process
process = Process <$ reserved "proc" <*> var <*> block

program :: Parser Program
program = Program <$> many process <* eof
