{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Lang.System.Sizer where

import Lang.System.AST
import Lang.System.Unifier
import Lang.System.Unique
import Control.Lens hiding (Empty)
import Data.List (genericLength)
import Data.Maybe (fromJust)

instance Unifiable Size Ident where
    viewut (ConstSize _) = ConstTerm
    viewut t@(Ptr s) = FuncTerm t [s]
    viewut (SizeVar v) = VarTerm v
    viewut t@(FunctionSize s1 s2) = FuncTerm t [s1, s2]
    viewut t@(ArraySize s _) = FuncTerm t [s]
    viewut t@(TupleSize ss) = FuncTerm t ss
    viewut t@(PolySize ss) = FuncTerm t ss

    substitute x t1 t2 =
        let sub = substitute x t1 in
        case t2 of
          (SizeVar y) -> if x == y then t1 else t2
          (ConstSize _) -> t2
          (Ptr s) -> Ptr $ sub s
          (FunctionSize s1 s2) -> FunctionSize (sub s1) (sub s2)
          (ArraySize s n) -> ArraySize (sub s) n
          (TupleSize ss) -> TupleSize $ sub <$> ss
          (PolySize ss) -> PolySize $ sub <$> ss

type SizedExpr = Expr Size

instance Uniquable Size where
    ustream = map (SizeVar . ("s" ++) . show) [0..]

annotateSizes :: Raw -> SizedExpr
annotateSizes e = runUnique (traverse (const fresh) e) ([] :: [Size])

type SizeConstraint = (Size, Size)

generateConstraints :: SizedExpr -> [SizeConstraint]
generateConstraints (Literal s l) = [(s, numericSize)]
generateConstraints (Sizeof s1 s2) = [(s1, numericSize)]
generateConstraints (Var s x) = [] -- no constraints since can be any size
generateConstraints (Builtin s Not e) =
    (s, view _info e)
    : generateConstraints e
generateConstraints (Builtin s b e) =
    [ (view _info e1, view _info e2)
    , (view _info e1, s)
    , (view _info e2, s) ]
    ++ generateConstraints e1
    ++ generateConstraints e2
        -- these are irrefutable due to the Builtin
        where e1 = fromJust (preview (_Tuple . _2) e) !! 1
              e2 = fromJust (preview (_Tuple . _2) e) !! 2
generateConstraints (App s e1 e2) =
    [ (view _info e1, FunctionSize (view _info e2) s) ]
    ++ generateConstraints e1
    ++ generateConstraints e2
generateConstraints (Cond s e1 e2 e3) =
    [ (view _info e1, ConstSize Int)
    , (view _info e2, view _info e3)
    , (view _info e2, s)
    , (view _info e3, s) ]
    ++ generateConstraints e1
    ++ generateConstraints e2
    ++ generateConstraints e3
generateConstraints (While s e1 e2) =
    [ (view _info e1, ConstSize Int)
    , (view _info e2, s) ]
    ++ generateConstraints e1
    ++ generateConstraints e2
generateConstraints (Block s p) =
    (s, ConstSize Unsized)
    : (programExpressions p >>= generateConstraints)
generateConstraints (Tuple s es) =
    (s, TupleSize $ map (view _info) es)
    : (es >>= generateConstraints)
generateConstraints (Array s es) =
    map (mkConstraint . view _info) es
    ++ (es >>= generateConstraints)
        where l = genericLength es
              mkConstraint s' = (ArraySize s' l, s)
generateConstraints (Index s e n) =
    undefined
    -- not quite sure how to do this
    -- s = e1[e2] but I can't compute e1[e2] statically
    -- deciding between tuples and lists is also tricky
generateConstraints (Ref s e) = (s, Ptr (view _info e)) : generateConstraints e
generateConstraints (Deref s e) = (view _info e, Ptr s) : generateConstraints e
generateConstraints (Assign s e1 e2) =
    [ (view _info e1, Ptr (view _info e2))
    , (s, ConstSize Empty) ]
    ++ generateConstraints e1
    ++ generateConstraints e2

sizeCheck :: SizedExpr -> Maybe SizedExpr
sizeCheck se = traverse ((unify constraints <*>) . pure) se
    where constraints = generateConstraints se
