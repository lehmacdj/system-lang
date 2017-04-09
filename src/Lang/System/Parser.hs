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
        , "~", "|", "&", ">>", "<<"
        , "@", "!"
        , "=", "$" ]
    , P.reservedNames =
        [ "if", "then", "else"
        , "while", "do" ]
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
    [ [ binopl "*" Mult ]
    , [ binopl "+" Add, binopl "-" Sub ]
    , [ binopl ">" Gt, binopl "=" Eq ]
    , [ prefix "~" Not ]
    ]
    compoundExpression

compoundExpression :: Parser Expr
compoundExpression =
    fmap Constant constant
    <|> fmap Var identifier
    <|> parens compoundExpression

constant :: Parser Value
constant = fromInteger <$> integer

block :: Parser Statement
block = Block <$> braces (semiSep1 statement)

assignment :: Parser Statement
assignment = do
    x <- identifier
    reservedOp ":="
    e <- expression
    pure $ Assign x e

label :: Parser Statement
label = Label <$> (string ":" *> identifier <* string ":")

conditional :: Parser Statement
conditional = do
    reserved "when"
    e <- expression
    s <- statement
    pure $ Cond s e

jump :: Parser Statement
jump = Jump <$> (reserved "jump" *> identifier)

statement :: Parser Statement
statement = block <|> assignment <|> label <|> conditional <|> jump

readExpr :: String -> Either ParseError Expr
readExpr = parse (whiteSpace *> expression <* eof) "system-expr"

readStatement :: String -> Either ParseError Statement
readStatement = parse (whiteSpace *> statement <* eof) "system-statement"
