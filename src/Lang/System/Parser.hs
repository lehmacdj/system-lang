module Lang.System.Parser where

import Lang.System.AST
import Text.Parsec hiding (label)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity (Identity)
import Control.Applicative hiding ((<|>), optional)

type Parser = Parsec String ()

tokenParser :: P.TokenParser u
tokenParser = P.makeTokenParser emptyDef
    { P.commentLine = "#"
    , P.identStart = letter
    , P.identLetter = alphaNum
    , P.opStart = oneOf "+*-:><=/&|~"
    , P.opLetter = oneOf "+*-:><=/&|~"
    , P.reservedOpNames =
        [ "+", "-", "*", "/"
        , "==", ">=", "<=", ">", "<", "/="
        , "~", "|", "^", "&", ">>", "<<"
        , "&&", "||"
        , ":="
        ]
    , P.reservedNames =
        [ "if", "then", "else"
        , "struct"
        , "goto", "call"
        , "1b", "0b"
        ]
    , P.caseSensitive = True
    }

P.TokenParser
    { P.reservedOp = reservedOp
    , P.reserved = reserved
    , P.parens = parens
    , P.semiSep1 = semiSep1
    , P.identifier = identifier
    , P.whiteSpace = whiteSpace
    , P.integer = integer
    , P.braces = braces
    , P.brackets = brackets
    , P.commaSep1 = commaSep1
    } = tokenParser

binop :: String -> (a -> a -> a) -> Assoc -> Operator String u Identity a
binop name fn = Infix ((reservedOp name <|> reserved name) *> pure fn)

binopl :: String -> Operator String u Identity Expr
binopl name = binop name (Binop name) AssocLeft

prefix :: String -> (a -> a) -> Operator String u Identity a
prefix name fn = Prefix ((reservedOp name <|> reserved name) *> pure fn)

binope :: Parser Expr
binope = buildExpressionParser
    [ [ binopl "*", binopl "/" ]
    , [ binopl "+", binopl "-" ]
    , [ binopl "|", binopl "^", binopl "&"
      , binopl ">>", binopl "<<" ]
    , [ binopl ">=", binopl "<="
      , binopl "<", binopl ">"
      , binopl "==", binopl "/=" ]
    , [ binopl "&&", binopl "||" ]
    , [ binopl ":=" ]
    ]
    expr

tuple :: Parser a -> Parser [a]
tuple p = parens (p `sepEndBy1` string ",")

label :: Parser Label
label = do
    l <- identifier
    params <- optionMaybe (tuple identifier)
    string ":"
    pure $ maybe (ULabel l) (PLabel l) params

offset :: Parser Offset
offset = fmap IOff integer <|> fmap LOff identifier

indexe :: Parser Expr
indexe = do
    e <- expr
    string "["
    o <- offset
    string "]"
    pure $ Index e o

arraye :: Parser Expr
arraye = Array <$> brackets (commaSep1 expr)

codee :: Parser Expr
codee = Code <$> braces (semiSep1 statement)

tuplee :: Parser Expr
tuplee = Tuple <$> tuple expr

calle :: Parser Expr
calle = do
    reserved "call"
    l <- identifier
    e <- optionMaybe tuplee
    pure $ ECall l e

constant :: Parser Constant
constant = reserved "1b" *> pure I
    <|> reserved "0b" *> pure O
    <|> string "b" *> fmap (Byte . fromInteger) integer
    <|> fmap (I32 . fromInteger) integer

expr :: Parser Expr
expr = parens binope
    <|> fmap Constant constant
    <|> indexe
    <|> arraye
    <|> codee
    <|> tuplee
    <|> calle
    <|> fmap Var identifier


gotos :: Parser Statement
gotos = do
    reserved "goto"
    l <- identifier
    e <- optionMaybe tuplee
    pure $ Jump l e

calls :: Parser Statement
calls = do
    reserved "call"
    l <- identifier
    e <- optionMaybe tuplee
    pure $ SCall l e

assigns :: Parser Statement
assigns = do
    x <- identifier
    reservedOp ":="
    e <- expr
    pure $ Assign x e

conds :: Parser Statement
conds = do
    reserved "if"
    e1 <- expr
    reserved "then"
    s1 <- statement
    reserved "else"
    s2 <- statement
    pure $ Cond e1 s1 s2

blocks :: Parser Statement
blocks = Block <$> braces (semiSep1 statement)

labeledStatement :: Parser Statement
labeledStatement = fmap Labeled label <*> statement

statement :: Parser Statement
statement = blocks
    <|> gotos
    <|> calls
    <|> assigns
    <|> conds
    <|> labeledStatement

readExpr :: String -> Either ParseError Expr
readExpr = parse (whiteSpace *> expr <* eof) "system-expr"

readStatement :: String -> Either ParseError Statement
readStatement = parse (whiteSpace *> statement <* eof) "system-statement"

readLabel :: String -> Either ParseError Label
readLabel = parse (whiteSpace *> label <* eof) "system-label"

readConstant :: String -> Either ParseError Constant
readConstant = parse (whiteSpace *> constant <* eof) "system-constant"

readOffset :: String -> Either ParseError Offset
readOffset = parse (whiteSpace *> offset <* eof) "system-offset"
