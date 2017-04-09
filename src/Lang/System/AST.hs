module Lang.System.AST where

import Data.Word
import Text.Printf
import Data.List

type Literal = Integer

data Expr
    -- arithmatic
    = Add Expr Expr
    | Mult Expr Expr
    | Sub Expr Expr
    | Div Expr Expr

    -- bitwise
    | Shl Expr Expr
    | Shr Expr Expr
    | And Expr Expr
    | Or Expr Expr
    | Not Expr

    -- comparisons
    | Gt Expr Expr
    | Geq Expr Expr
    | Lt Expr Expr
    | Leq Expr Expr
    | Eq Expr Expr
    | Neq Expr Expr

    -- abstractions
    | App Expr Expr
    | Cond Expr Expr Expr
    | While Expr Expr

    -- compound expressions
    | Block [Expr]
    | Tuple [Expr]
    | Array [Expr]
    | Index Expr Expr

    -- references
    | Ref Expr
    | Deref Expr

    -- assignment
    | Assign Expr Expr

    -- required but not involved in parsing
    | Var String
    | Literal Literal

instance Show Expr where
    -- arithmatic
    show (Add e1 e2) = undefined
    show (Mult e1 e2) = undefined
    show (Sub e1 e2) = undefined
    show (Div e1 e2) = undefined

    -- bitwise
    show (Shl e1 e2) = undefined
    show (Shr e1 e2) = undefined
    show (And e1 e2) = undefined
    show (Or e1 e2) = undefined
    show (Not e) = undefined

    -- comparisons
    show (Gt e1 e2) = undefined
    show (Geq e1 e2) = undefined
    show (Lt e1 e2) = undefined
    show (Leq e1 e2) = undefined
    show (Eq e1 e2) = undefined
    show (Neq e1 e2) = undefined

    -- abstractions
    show (App e1 e2) = undefined
    show (Cond e1 e2 e3) = undefined
    show (While e1 e2) = undefined

    -- compound expressions
    show (Block es) = undefined
    show (Tuple es) = undefined
    show (Array es) = undefined
    show (Index e1 e2) = undefined

    -- references
    show (Ref e) = undefined
    show (Deref e) = undefined

    -- assignment
    show (Assign e1 e2) = undefined
    show (Var x) = undefined
    show (Literal l) = undefined
