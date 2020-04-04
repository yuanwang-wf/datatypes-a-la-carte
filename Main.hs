{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

newtype Expr f = In (f (Expr f))

data CoProduct f g e = Inl (f e) | Inr (g e)

-- e is phantom type for type safey
newtype Val e = Val Int

type IntExpr = Expr Val

data Add e = Add e e

type AddExpr = Expr Add

data Mul x = Mul x x

instance Functor Val where
  fmap _ (Val e) = Val e

instance Functor Add where
  fmap f (Add left right) = Add (f left) (f right)

instance Functor Mul where
  fmap f (Mul left right) = Mul (f left) (f right)

instance (Functor f, Functor g) => Functor (CoProduct f g) where
  fmap h (Inl l) = Inl (fmap h l)
  fmap h (Inr r) = Inr (fmap h r)

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add l r) = l + r

instance (Eval f, Eval g) => Eval (CoProduct f g) where
  evalAlgebra (Inl l) = evalAlgebra l
  evalAlgebra (Inr r) = evalAlgebra r

instance Eval Mul where
  evalAlgebra (Mul l r) = l * r

eval :: Eval f => Expr f -> Int
eval = foldExpr evalAlgebra

class (Functor sub, Functor sup) => Member sub sup where
  inj :: sub a -> sup a

instance Functor f => Member f f where
  inj = id

instance (Functor f, Functor g) => Member f (CoProduct f g) where
  inj = Inl

instance (Functor f, Functor g) => Member g (CoProduct f g) where
  inj = Inr

--instance (Functor f, Functor g, Functor h, Member f g) => Member f (CoProduct h g) where
--  inj = Inr . inj

inject :: Member g f => g (Expr f) -> Expr f
inject = In . inj

val :: Member Val f => Int -> Expr f
val x = inject (Val x)

infixl 6 ⊕

(⊕) :: Member Add f => Expr f -> Expr f -> Expr f
x ⊕ y = inject (Add x y)

infixl 7 ⊗

(⊗) :: Member Mul f => Expr f -> Expr f -> Expr f
x ⊗ y = inject (Mul x y)

main :: IO ()
main = print $ eval x
  where
    x :: Expr (CoProduct Mul Val)
    x = val 80 ⊗ val 5
