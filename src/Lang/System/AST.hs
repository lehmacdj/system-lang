{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}

module Lang.System.AST where

import Data.Word
import Text.Printf
import Data.List
import Data.Bifunctor

type Literal = Integer

type Ident = String

data Builtin = Add | Sub | Mult | Div
             | And | Or | Xor | Not | Shl | Shr
             | Gt | Geq | Lt | Leq | Eq | Neq
             deriving (Show)

type Raw = Expr ()
type RawStmt = Statement ()
type RawProg = Program ()

type Program a = [Statement a]

-- a declaration of a variable
data Statement a
    = SizeDecl Ident Size
    | ExprDecl Ident (Expr a)
    | EvalExpr (Expr a)
    deriving (Functor, Traversable, Show, Foldable)

data Expr a
    = Literal a Literal
    | Sizeof a Size
    | Var a Ident
    | Builtin a Builtin (Expr a)
    -- control flow
    | App a (Expr a) (Expr a)
    | Cond a (Expr a) (Expr a) (Expr a)
    | While a (Expr a) (Expr a)
    -- compound expressions
    | Block a (Program a)
    | Tuple a [Expr a]
    | Array a [Expr a]
    | Index a (Expr a) (Expr a)
    -- references
    | Ref a (Expr a)
    | Deref a (Expr a)
    -- assignment
    | Assign a (Expr a) (Expr a)
    deriving (Show, Functor, Foldable, Traversable)

isValue :: Expr a -> Bool
isValue (Literal _ _) = True
isValue (Var _ _) = True
isValue (Block _ _) = True
isValue (Tuple _ es) = all isValue es
isValue (Array _ es) = all isValue es
isValue _ = False

data ConstSize = Bit
               | Byte
               | Int
               | Empty
               | Unsized
               deriving (Show)

data Size = ConstSize ConstSize
          | Ptr Size
          | SizeVar Ident
          | FunctionSize Size Size
          | ArraySize Size Integer
          | TupleSize [Size]
          deriving (Show)

instance Eq Size where
    a == b = sizeof a == sizeof b

sizeof :: Size -> Integer
sizeof (ConstSize Empty) = 0
sizeof (ConstSize Bit) = 1
sizeof (ConstSize Byte) = 8
sizeof (ConstSize Int) = 32
sizeof (ConstSize Unsized) = undefined
sizeof (Ptr _) = 64
sizeof (FunctionSize _ _) = 64
sizeof (ArraySize s n) = sizeof s * n
sizeof (TupleSize ss) = sum $ sizeof <$> ss
