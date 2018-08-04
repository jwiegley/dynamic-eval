{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Lambda where

import Bound
import Control.Monad (ap)
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Functor.Classes
import Reflex

data Lam a
    = Var a
    | App (Lam a) (Lam a)
    | Abs (Scope () Lam a)
    deriving (Functor, Foldable, Traversable)

instance Applicative Lam where
    pure = Var
    (<*>) = ap

instance Monad Lam where
    return = Var

    Var a   >>= f = f a
    App x y >>= f = App (x >>= f) (y >>= f)
    Abs e   >>= f = Abs (e >>>= f)

deriveEq1   ''Lam
deriveOrd1  ''Lam
deriveRead1 ''Lam
deriveShow1 ''Lam

instance Eq a   => Eq   (Lam a) where (==) = eq1
instance Ord a  => Ord  (Lam a) where compare = compare1
instance Show a => Show (Lam a) where showsPrec = showsPrec1
instance Read a => Read (Lam a) where readsPrec = readsPrec1

lam :: Eq a => a -> Lam a -> Lam a
lam v b = Abs (abstract1 v b)

whnf :: Lam a -> Lam a
whnf (App f a) = case whnf f of
  Abs b -> whnf (instantiate1 a b)
  f'    -> App f' a
whnf e = e
