{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- https://typeclasses.com/ghc/type-operators
{-# LANGUAGE TypeOperators #-}

module Main where

import GHC.Generics

newtype Expr f = In (f (Expr f))

--data (f :+: g) e = Inl (f e) | Inr (g e)

--infix 8 :+:

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

--instance (Functor f, Functor g) => Functor (f :+:  g) where
--  fmap h (Inl l) = Inl (fmap h l)
--  fmap h (Inr r) = Inr (fmap h r)

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add l r) = l + r

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (L1 l) = evalAlgebra l
  evalAlgebra (R1 r) = evalAlgebra r

instance Eval Mul where
  evalAlgebra (Mul l r) = l * r

eval :: Eval f => Expr f -> Int
eval = foldExpr evalAlgebra

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f =>  f :<: f where
  inj = id

instance (Functor f, Functor g) => f :<: ( f :+: g) where
  inj = L1

--instance (Functor f, Functor g) =>  g :<: ( f :+: g) where
--  inj = Inr


instance {-# OVERLAPPABLE #-}
         (Functor f, Functor g, Functor h, f :<: g) => f :<: ( h :+: g) where
  inj = R1 . inj

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

val ::  (Val :<: f) => Int -> Expr f
val x = inject (Val x)

infixl 6 ⊕

(⊕) :: (Add :<: f) => Expr f -> Expr f -> Expr f
x ⊕ y = inject (Add x y)

infixl 7 ⊗

(⊗) :: (Mul :<: f) => Expr f -> Expr f -> Expr f
x ⊗ y = inject (Mul x y)



main :: IO ()
main = print $ eval x
 where x :: Expr (Val :+: Add :+: Mul)
       x = val 80  ⊗ val 5 ⊕ val 4
