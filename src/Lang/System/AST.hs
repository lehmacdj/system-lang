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
    | Xor Expr Expr
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
