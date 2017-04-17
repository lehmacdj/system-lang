{-# LANGUAGE ViewPatterns, MultiParamTypeClasses, FunctionalDependencies #-}

module Lang.System.Unifier
    ( VarContaining(..)
    , Unifiable(..)
    , UnifiableView(..)
    , unify
    ) where

import Control.Lens
import Control.Applicative

class Eq v => VarContaining t v | t -> v where
    fvs :: t -> [v]
    allvs :: t -> [v]

data UnifiableView t v
    = ConstTerm
    | VarTerm v
    | FuncTerm t [t]

class (Eq t, VarContaining t v) => Unifiable t v | t -> v where
    -- substitute t1 for t2
    substitute :: v -> t -> t -> t
    -- a view of t
    viewut :: t -> UnifiableView t v

unify :: Unifiable t v => [(t, t)] -> Maybe (t -> t)
unify [] = Just id
unify ((s@(viewut -> VarTerm v), t):e)
  | v `elem` fvs t = Nothing
  | otherwise =
      let sub = substitute v s
       in (sub .) <$> unify (over (traverse . both) sub e)
unify ((s, t@(viewut -> VarTerm v)):e)
  | v `elem` fvs s = Nothing
  | otherwise =
      let sub = substitute v t
       in (sub .) <$> unify (over (traverse . both) sub e)
unify ((viewut -> FuncTerm f ss, viewut -> FuncTerm g ts):e)
  | f == g = (.) <$> unify (zip ss ts) <*> unify e
  | otherwise = Nothing
unify ((s, t):e)
  | s == t = unify e
  | otherwise = Nothing
