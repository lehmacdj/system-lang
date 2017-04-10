module Lang.System.Parser where

import Lang.System.AST
import Text.Parsec hiding (Empty)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity (Identity)
import Control.Applicative hiding ((<|>), optional)
import Data.List (tails)
import Control.Monad

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
        , "sizeof"
        , "Empty", "Bit", "Byte", "Any", "Unsized" ]
    , P.caseSensitive = True
    }

P.TokenParser
    { P.reservedOp = reservedOp
    , P.reserved = reserved
    , P.parens = parens
    , P.identifier = identifier
    , P.whiteSpace = whiteSpace
    , P.integer = integer
    , P.braces = braces
    , P.brackets = brackets
    , P.lexeme = lexeme
    , P.comma = comma
    , P.semi = semi
    } = tokenParser

semiSepEndBy1 :: Parser a -> Parser [a]
semiSepEndBy1 p = lexeme p `sepEndBy` semi

commaSepEndBy :: Parser a -> Parser [a]
commaSepEndBy p = lexeme p `sepEndBy` comma

commaSepEndBy1 :: Parser a -> Parser [a]
commaSepEndBy1 p = lexeme p `sepEndBy1` comma

-- combine two expressions into a tuple
tuplify :: Expr -> Expr -> Expr
tuplify e1 e2 = Tuple [e1, e2]

binBuiltin :: Builtin -> Expr -> Expr -> Expr
binBuiltin b e1 e2 = Builtin b (tuplify e1 e2)

binop :: String -> (a -> a -> a) -> Assoc -> Operator String u Identity a
binop name fn = Infix ((reservedOp name <|> reserved name) *> pure fn)

binopl :: String -> (a -> a -> a) -> Operator String u Identity a
binopl name fn = binop name fn AssocLeft

binopr :: String -> (a -> a -> a) -> Operator String u Identity a
binopr name fn = binop name fn AssocRight

prefix :: String -> (a -> a) -> Operator String u Identity a
prefix name fn = Prefix ((reservedOp name <|> reserved name) *> pure fn)

-- a magic thing that works like build expression parser but is better
-- specifically supports repeated prefix operators
-- http://stackoverflow.com/questions/33214163/parsec-expr-repeated-prefix-with-different-priority/33534426#33534426
buildPrattParser table termP = parser precs where

  precs = reverse table

  prefixP = choice prefixPs <|> termP where
    prefixPs = do
      precsR@(ops:_) <- tails precs
      Prefix opP <- ops
      return $ opP <*> parser precsR

  infixP precs lhs = choice infixPs <|> pure lhs where
    infixPs = do
      precsR@(ops:precsL) <- tails precs
      op <- ops
      p <- case op of
        Infix opP assoc -> do
          let p precs = opP <*> pure lhs <*> parser precs
          return $ case assoc of
            AssocNone  -> error "Non associative operators are not supported"
            AssocLeft  -> p precsL
            AssocRight -> p precsR
        Postfix opP ->
          return $ opP <*> pure lhs
        Prefix _ -> mzero
      return $ p >>= infixP precs

  parser precs = prefixP >>= infixP precs

expression :: Parser Expr
expression =
    applications
    <|> buildPrattParser
        [ [ prefix "~" (Builtin Not), prefix "@" Ref, prefix "!" Deref ]
        , [ binopl "*" (binBuiltin Mult), binopl "/" (binBuiltin Div) ]
        , [ binopl "+" (binBuiltin Add), binopl "-" (binBuiltin Sub) ]
        , [ binopl ">>" (binBuiltin Shr), binopl "<<" (binBuiltin Shl) ]
        , [ binopl "&" (binBuiltin And) ]
        , [ binopl "^" (binBuiltin Xor) ]
        , [ binopl "|" (binBuiltin Or) ]
        , [ binopl ">=" (binBuiltin Geq), binopl "<=" (binBuiltin Leq)
          , binopl ">" (binBuiltin Gt), binopl "<" (binBuiltin Lt)
          , binopl "==" (binBuiltin Eq), binopl "~=" (binBuiltin Neq) ]
        , [ binopl "=" Assign ]
        ]
        compoundExpression

compoundExpression :: Parser Expr
compoundExpression =
    try tuple
    <|> parens expression
    <|> Literal <$> literal
    <|> Var <$> identifier
    <|> Sizeof <$> (reserved "sizeof" *> size)
    <|> conditional
    <|> while
    <|> block
    <|> array

literal :: Parser Literal
literal = fromInteger <$> integer

applications :: Parser Expr
applications = foldl1 App <$> compoundExpression `sepBy1` whiteSpace

block :: Parser Expr
block = Block <$> braces (semiSepEndBy1 expression)

tuple :: Parser Expr
tuple = Tuple <$> parens ((:) <$> expression <* comma <*> commaSepEndBy expression)

array :: Parser Expr
array = Array <$> brackets (commaSepEndBy1 expression)

conditional :: Parser Expr
conditional = do
    reserved "if"
    e1 <- expression
    reserved "then"
    e2 <- expression
    reserved "else"
    e3 <- expression
    pure $ Cond e1 e2 e3

while :: Parser Expr
while = do
    reserved "while"
    e1 <- expression
    reserved "do"
    e2 <- expression
    pure $ While e1 e2


size :: Parser Size
size = buildPrattParser
    [ [ prefix "@" Ptr ]
    , [ binopr "->" FunctionSize ]
    ]
    compoundSize

compoundSize :: Parser Size
compoundSize =
    try tupleSize
    <|> parens size
    <|> reserved "Empty" *> pure Empty
    <|> reserved "Bit" *> pure Bit
    <|> reserved "Byte" *> pure Byte
    <|> reserved "Any" *> pure Any
    <|> reserved "Unsized" *> pure Unsized
    <|> UserSize <$> identifier
    <|> arraySize

tupleSize :: Parser Size
tupleSize = TupleSize <$> parens (commaSepEndBy1 size)

arraySize :: Parser Size
arraySize = brackets (ArraySize <$> size <* semi <*> integer)

readExpr :: String -> Either ParseError Expr
readExpr = parse (whiteSpace *> expression <* eof) "system-expr"

readSize :: String -> Either ParseError Size
readSize = parse (whiteSpace *> size <* eof) "system-size"
