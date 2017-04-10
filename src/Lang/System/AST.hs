module Lang.System.AST where

import Data.Word
import Text.Printf
import Data.List

type Literal = Integer

data Builtin = Add | Sub | Mult | Div
             | And | Or | Xor | Not | Shl | Shr
             | Gt | Geq | Lt | Leq | Eq | Neq
             deriving (Show)

data Expr
    = Builtin Builtin Expr

    -- control flow
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

    -- required but not involved in evaluation
    | Var String
    | Literal Literal
    -- | SizeCoerce Expr Size
    deriving (Show)

isValue :: Expr -> Bool
isValue (Literal _) = True
isValue (Var _) = True
isValue (Block _) = True
isValue (Tuple es) = all isValue es
isValue (Array es) = all isValue es
isValue _ = False
