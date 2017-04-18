{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Lang.System.AST where

import Lang.System.Unifier (VarContaining(..))

import Data.Word
import Text.Printf
import Data.List
import Data.Bifunctor
import Data.Maybe (mapMaybe)
import Control.Lens (lens, Lens', prism, Prism', Prism)

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

programExpressions :: Program a -> [Expr a]
programExpressions = mapMaybe stmtExpr
    where stmtExpr (SizeDecl _ _) = Nothing
          stmtExpr (ExprDecl _ e) = Just e
          stmtExpr (EvalExpr e) = Just e

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
    | Index a (Expr a) Literal
    -- references
    | Ref a (Expr a)
    | Deref a (Expr a)
    -- assignment
    | Assign a (Expr a) (Expr a)
    deriving (Show, Functor, Foldable, Traversable)

instance VarContaining (Expr a) Ident where
    allvs (Var _ x) = [x]
    allvs e = subexpressions e >>= allvs
    fvs = allvs

subexpressions :: Expr a -> [Expr a]
subexpressions (Literal _ _) = []
subexpressions (Sizeof _ _) = []
subexpressions (Var _ _) = []
subexpressions (Builtin _ _ e) = [e]
subexpressions (App _ e1 e2) = [e1, e2]
subexpressions (Cond _ e1 e2 e3) = [e1, e2, e3]
subexpressions (While _ e1 e2) = [e1, e2]
subexpressions (Block _ p) = programExpressions p
subexpressions (Tuple _ es) = es
subexpressions (Array _ es) = es
subexpressions (Index _ e n) = [e]
subexpressions (Ref _ e) = [e]
subexpressions (Deref _ e) = [e]
subexpressions (Assign _ e1 e2) = [e1, e2]

_Tuple :: Prism' (Expr a) (a, [Expr a])
_Tuple = prism (uncurry Tuple) project where
    project (Tuple i es) = Right (i, es)
    project e = Left e

_Array :: Prism' (Expr a) (a, [Expr a])
_Array = prism (uncurry Array) project where
    project (Array i es) = Right (i, es)
    project e = Left e

_info :: Lens' (Expr a) a
_info = lens get set where
    get :: Expr a -> a
    get (Literal x _) = x
    get (Sizeof x _) = x
    get (Var x _) = x
    get (Builtin x _ _) = x
    get (App x _ _) = x
    get (Cond x _ _ _) = x
    get (While x _ _) = x
    get (Block x _) = x
    get (Tuple x _) = x
    get (Array x _) = x
    get (Index x _ _) = x
    get (Ref x _) = x
    get (Deref x _) = x
    get (Assign x _ _) = x

    set :: Expr a -> a -> Expr a
    set (Literal x e) x' = Literal x' e
    set (Sizeof x s) x' = Sizeof x' s
    set (Var x i) x' = Var x' i
    set (Builtin x b e) x' = Builtin x' b e
    set (App x e1 e2) x' = App x' e1 e2
    set (Cond x e1 e2 e3) x' = Cond x' e1 e2 e3
    set (While x e1 e2) x' = While x' e1 e2
    set (Block x p) x' = Block x' p
    set (Tuple x e) x' = Tuple x' e
    set (Array y e) x' = Array x' e
    set (Index x e1 e2) x' = Index x' e1 e2
    set (Ref x e) x' = Ref x' e
    set (Deref x e) x' = Deref x' e
    set (Assign x e1 e2) x' = Assign x' e1 e2

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
               deriving (Show, Eq)

data Size = ConstSize ConstSize
          | Ptr Size
          | SizeVar Ident
          | FunctionSize Size Size
          | ArraySize Size Integer
          | TupleSize [Size]
          -- may not appear in programs, used internally and for literals
          | PolySize [Size]
          deriving (Show)

numericSize :: Size
numericSize = PolySize $ ConstSize <$> [Bit, Byte, Int]

instance Eq Size where
    (ConstSize a) == (ConstSize b) = a == b
    (Ptr a) == (Ptr b) = a == b
    (SizeVar a) == (SizeVar b) = a == b
    (FunctionSize a r) == (FunctionSize a' r') = a == a' && r == r'
    (ArraySize a n) == (ArraySize a' n') = a == a' && n == n'
    (TupleSize ss) == (TupleSize ss') = ss == ss'
    (PolySize ss) == (PolySize ss')
      | length ss < length ss' = ss' `elem` permutations ss
      | otherwise = ss `elem` permutations ss'
    a == (PolySize ss) = a `elem` ss
    (PolySize ss) == b = b `elem` ss
    a == b = False

instance VarContaining Size Ident where
    allvs (ConstSize _) = []
    allvs (Ptr s) = allvs s
    allvs (FunctionSize s1 s2) = allvs s1 ++ allvs s2
    allvs (ArraySize s _) = allvs s
    allvs (TupleSize ss) = ss >>= allvs
    allvs (PolySize ss) = ss >>= allvs

    fvs = allvs

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
sizeof (PolySize ss) = undefined
