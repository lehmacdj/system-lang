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
subexpressions (Assign _ e1 e2) = [e1, e2]

_Tuple :: Prism' (Expr a) (a, [Expr a])
_Tuple = prism (uncurry Tuple) project where
    project (Tuple i es) = Right (i, es)
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
    set (Assign x e1 e2) x' = Assign x' e1 e2

isValue :: Expr a -> Bool
isValue (Literal _ _) = True
isValue (Var _ _) = True
isValue (Block _ _) = True
isValue (Tuple _ es) = all isValue es
isValue _ = False

data Size = Empty -- 0
          | Bit -- 1
          | Unsized
          | SizeVar Ident
          | SizeSum Size Size
          -- is a size morphism from 1 -> 2 with size 3
          | SizeMorph Size Size Size
          deriving (Show)

instance Eq Size where
    s == s' = sizeof s == sizeof s'

sizeof :: Size -> Maybe Integer
sizeof Empty = Just 0
sizeof Bit = Just 1
sizeof (SizeVar _) = Nothing
sizeof Unsized = Nothing
sizeof (SizeSum s t) = (+) <$> sizeof s <*> sizeof t
sizeof (SizeMorph _ _ s) = sizeof s
