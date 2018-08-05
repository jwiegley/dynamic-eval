{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module Lambda where

import Bound
import Control.Applicative
import Control.Monad
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Functor.Classes
import Control.Monad.Trans.Class
import Reflex
import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Functor

data LamF f a
    = Var a
    | App (f a) (f a)
    | Abs (Scope () f a)
    deriving (Functor, Foldable, Traversable)

newtype Lam a = Lam { unLam :: LamF Lam a } deriving (Functor, Applicative, Monad)

newtype LamD t a = LamD { unLamD :: Dynamic t (LamF (LamD t) a) }

deriving instance Reflex t => Functor (LamD t)

instance Reflex t => Applicative (LamD t) where
  pure = LamD . pure . Var
  (<*>) = ap

instance Reflex t => Monad (LamD t) where
  return = pure
  x >>= f = joinLamD $ fmap f x

instance Applicative (LamF Lam) where
    pure = Var
    (<*>) = ap

instance Monad (LamF Lam) where
    return = pure

    Var a   >>= f = f a
    App x y >>= f = App (x >>= Lam . f) (y >>= Lam . f)
    Abs e   >>= f = Abs (e >>>= Lam . f)

joinLamD :: Reflex t => LamD t (LamD t a) -> LamD t a
joinLamD (LamD v) = LamD $ v >>= \case
  Var (LamD a) -> a
  App x y -> pure $ App (joinLamD x) (joinLamD y)
  Abs (Scope e) -> pure $ Abs $ Scope $ joinLamD $ fmap sequence e

--deriveEq1   ''LamF
--deriveOrd1  ''LamF
--deriveRead1 ''LamF
--deriveShow1 ''LamF

--instance Eq a   => Eq   (Lam a) where (==) = eq1
--instance Ord a  => Ord  (Lam a) where compare = compare1
--instance Show a => Show (Lam a) where showsPrec = showsPrec1
--instance Read a => Read (Lam a) where readsPrec = readsPrec1

lam :: Eq a => a -> Lam a -> Lam a
lam v b = Lam (Abs (abstract1 v b))

whnf :: Lam a -> Lam a
whnf (Lam (App f a)) = case whnf f of
  Lam (Abs b) -> whnf (instantiate1 a b)
  f'          -> Lam (App f' a)
whnf e = e

whnfD :: Reflex t => LamD t a -> LamD t a
whnfD l = LamD $ unLamD l >>= \case
  App f a -> unLamD (whnfD f) >>= \case
    Abs b -> unLamD $ whnfD $ instantiate1 a b
    f'   -> pure $ App (LamD (pure f')) a
  v -> pure v

data LamForm = VarForm | AppForm | AbsForm deriving (Show, Read, Eq, Ord, Enum, Bounded)

editLamD :: forall t m a. MonadWidget t m => (Dynamic t Text -> Dynamic t a) -> m (LamD t a)
editLamD getVar = do
  let form0 = VarForm
  form <- dropdown form0 (pure (Map.fromList $ fmap (\x -> (x, T.pack $ show x)) [minBound..])) def
  let f :: LamForm -> m (Dynamic t (LamF (LamD t) a))
      f = \case
        VarForm -> fmap Var . getVar . value <$> inputElement def
        AppForm -> do
          text "("
          f <- editLamD getVar
          text " "
          x <- editLamD getVar
          text ")"
          pure $ pure $ App f x
        AbsForm -> do
          text "(\\"
          bound <- value <$> inputElement def
          text " -> "
          let getVar' :: Dynamic t Text -> Dynamic t (Var () a)
              getVar' referenced = join $ liftA2 (\b r -> if b == r then pure (B ()) else F <$> getVar referenced) bound referenced
          b <- editLamD $ fmap (fmap (LamD . pure . Var)) . getVar'
          text ")"
          pure $ pure $ Abs (Scope b)
  LamD . join <$> widgetHold (f form0) (f <$> updated (value form))

viewLamD :: MonadWidget t m => Int -> (a -> m ()) -> LamD t a -> m ()
viewLamD depth showVar (LamD l) = dyn_ $ l <&> \case
  Var a -> showVar a
  App f x -> do
    text "("
    viewLamD depth showVar f
    text " "
    viewLamD depth showVar x
    text ")"
  Abs (Scope b) -> do
    let f = \case
          B _ -> text $ T.pack $ show depth
          F v -> viewLamD (succ depth) showVar v
    viewLamD (succ depth) f b

main :: IO ()
main = mainWidget $ do
  el "h1" $ text "Input"
  exp <- el "div" $ editLamD id
  el "h1" $ text "Raw"
  viewLamD 0 text exp
  el "h1" $ text "Reduced"
  viewLamD 0 text $ whnfD exp
  pure ()
