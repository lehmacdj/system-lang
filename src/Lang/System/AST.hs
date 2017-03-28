module Lang.System.AST where

import Data.Word
import Text.Printf
import Data.List

type Ident = String

type Var = Ident

data Constant = I | O
              | Byte Word8
              | I32 Int

type Params = [Var]

data Label = PLabel Ident Params
           | ULabel Ident

-- only used to index arrays
data Offset = IOff Integer
            | LOff Ident

data Expr = Array [Expr]
          | Code [Statement]
          | Tuple [Expr]
          | Index Expr Offset
          | Constant Constant
          | Binop Ident Expr Expr
          | ECall Ident (Maybe Expr)
          | Var Var

data Statement = Jump Ident (Maybe Expr)
               | SCall Ident (Maybe Expr)
               | Labeled Label Statement
               | Assign Var Expr
               | Cond Expr Statement Statement
               | Block [Statement]


instance Show Constant where
    show I = "1b"
    show O = "0b"
    show (Byte w) = printf "0x%02x" w
    show (I32 i) = show i

instance Show Label where
    show (PLabel n ps) = printf "%s(%s):" n (intercalate "," ps)
    show (ULabel n) = n ++ ":"

instance Show Offset where
    show (IOff i) = show i
    show (LOff l) = l

instance Show Expr where
    show (Array es) = "[" ++ intercalate ", " (show <$> es) ++ "]"
    show (Code ss) = "{" ++ intercalate ";\n" (show <$> ss) ++ "}"
    show (Tuple es) = "(" ++ intercalate ", " (show <$> es) ++ ")"
    show (Index e o) = show e ++ "[" ++ show o ++ "]"
    show (Constant c) = show c
    show (Binop n e1 e2) = "(" ++ show e1 ++ show n ++ show e2 ++ ")"
    show (ECall l (Just e)) = "call " ++ l ++ "(" ++ show e ++ ")"
    show (ECall l Nothing) = "call " ++ l
    show (Var x) = x

instance Show Statement where
    show (Jump l (Just e)) = "jump " ++ l ++ show e
    show (Jump l Nothing) = "jump " ++ l
    show (SCall l (Just e)) = "call " ++ l ++ show e
    show (SCall l Nothing) = "call " ++ l
    show (Block ss) = "{" ++ intercalate ";\n" (show <$> ss) ++ "}"
    show (Labeled l s) = show l ++ "\n" ++ show s
    show (Assign v e) = v  ++ " := " ++ show e
    show (Cond e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
