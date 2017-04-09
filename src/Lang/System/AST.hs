module Lang.System.AST where

import Data.Word
import Text.Printf
import Data.List

type Value = Int

type Label = String

data Expr = Add Expr Expr
          | Mult Expr Expr
          | Sub Expr Expr
          | Gt Expr Expr
          | Eq Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Not Expr
          | Var String
          | Constant Value

data Statement = Jump Label
                 | Assign String Expr
                 | Cond Statement Expr
                 | Block [Statement]
                 | Label String

instance Show Expr where
    show (Add e1 e2) = show e1 ++ " + " ++ show e2
    show (Mult e1 e2) = show e1 ++ " * " ++ show e2
    show (Sub e1 e2) = show e1 ++ " - " ++ show e2
    show (Gt e1 e2) = show e1 ++ " > " ++ show e2
    show (Eq e1 e2) = show e1 ++ " = " ++ show e2
    show (Or e1 e2) = show e1 ++ " | " ++ show e2
    show (And e1 e2) = show e1 ++ " & " ++ show e2
    show (Not e) = "~ " ++ show e
    show (Var x) = x
    show (Constant c) = show c

instance Show Statement where
    show (Jump l) = "jump " ++ l
    show (Block ss) = "{" ++ intercalate ";\n" (show <$> ss) ++ "}"
    show (Assign v e) = v  ++ " := " ++ show e
    show (Cond s e) = "when " ++ show e ++ " " ++ show s
    show (Label l) = ":" ++ l ++ ":"
