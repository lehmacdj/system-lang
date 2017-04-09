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
    , P.opStart = oneOf "+*-><=/&|~$@!"
    , P.opLetter = oneOf "+*-><=/&|~$@!"
    , P.reservedOpNames =
        [ "+", "-", "*", "/"
        , "<", ">", "<=", ">=", "==", "~="
        , "~", "|", "&", "^", ">>", "<<"
        , "@", "!"
        , "->"
        , "=", "$" ]
    , P.reservedNames =
        [ "if", "then", "else"
        , "while", "do"
        , "unit", "bit", "byte" ]
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

binopl :: String -> (a -> a -> a) -> Operator String u Identity a
binopl name fn = binop name fn AssocLeft

prefix :: String -> (a -> a) -> Operator String u Identity a
prefix name fn = Prefix ((reservedOp name <|> reserved name) *> pure fn)

expression :: Parser Expr
expression = buildExpressionParser
    [ [ prefix "~" Not, prefix "@" Ref, prefix "!" Deref ]
    , [ binopl "*" Mult, binopl "/" Div ]
    , [ binopl "+" Add, binopl "-" Sub ]
    , [ binopl ">>" Shr, binopl "<<" Shl ]
    , [ binopl "&" And ]
    , [ binopl "^" Xor ]
    , [ binopl "|" Or ]
    , [ binopl ">=" Geq, binopl "<=" Leq, binopl ">" Gt, binopl "<" Lt
      , binopl "==" Eq, binopl "~=" Neq
      ]
    , [ binopl "=" Assign ]
    ]
    compoundExpression

compoundExpression :: Parser Expr
compoundExpression =
    try (parens compoundExpression)
    <|> fmap Literal literal
    <|> fmap Var identifier
    <|> conditional
    <|> assignment
    <|> applications -- this is pretty sketchy
    <|> try block
    <|> try array
    <|> try tuple

literal :: Parser Literal
literal = fromInteger <$> integer

applications :: Parser Expr
applications = foldl App <$> expression <*> some (parens expression)

block :: Parser Expr
block = Block <$> braces (semiSep1 expression)

tuple :: Parser Expr
tuple = Tuple <$> parens (commaSep1 expression)

array :: Parser Expr
array = Array <$> brackets (commaSep1 expression)

assignment :: Parser Expr
assignment = do
    x <- expression
    reservedOp "="
    e <- expression
    pure $ Assign x e

conditional :: Parser Expr
conditional = do
    reserved "if"
    e1 <- expression
    reserved "then"
    e2 <- expression
    reserved "else"
    e3 <- expression
    pure $ Cond e1 e2 e3

readExpr :: String -> Either ParseError Expr
readExpr = parse (whiteSpace *> expression <* eof) "system-expr"
