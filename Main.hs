{-# LANGUAGE EmptyCase#-}
{-# LANGUAGE TypeOperators#-}
module Main where

import GHC.Generics

data Free f a
  = Var a
  | Op (f (Free f a))


data Or k = Or k k
instance Functor Or where
  fmap f (Or xs ys) = Or (f xs) (f ys)

a :: Free Or Int
a = Op (Or (Var 3) (Var 4))

type Alg f a = f a -> a

-- simailr to foldExpr
eval :: Functor f => (f b -> b) -> (a -> b) -> Free f a -> b
eval _ gen (Var x) = gen x
eval alg gen (Op x) = alg (fmap (eval alg gen) x)

-- identity monad ? or Const
data Stop k = Stop
instance Functor Stop where
  fmap _ Stop = Stop

fail :: Free Stop a -> Maybe a
fail = eval alg gen where
  alg :: Stop (Maybe a) -> Maybe a
  alg Stop = Nothing

  gen :: a -> Maybe a
  gen = Just

once :: Free Or a -> a
once = eval alg gen where

  alg :: Or a -> a
  alg (Or xs _) = xs

  gen :: a -> a
  gen = id

b :: Free Stop Int
b = Var 3

c :: Free Stop Int
c = Op Stop

data Void k

instance Functor Void where
  fmap _ _ = undefined

run :: Free Void a -> a
run = eval alg id where
  alg :: Void a -> a
  alg x = case x of {}

d :: Free Void Int
d = Var 666

-- data (f :+ sig) a = Eff (f a) | Sig (sig a)
embed :: Functor g => (f (Free g a) -> Free g a) -> ((f :+: g) (Free g a) -> Free g a)
embed alg (L1 x) = alg x
embed _ (R1 x) = Op x

-- exception
fail' :: Functor f => Free (Stop :+: f) a -> Free f (Maybe a)
fail' = eval (embed alg) gen where
  gen x = Var (Just x)
  alg Stop = Var Nothing


instance Functor f => Functor (Free f) where
  fmap f (Var k) = Var (f k)
  fmap f (Op o) = Op $ (fmap . fmap) f o


instance Functor f => Applicative (Free f) where
  pure = Var

  Var f <*> o = fmap f o
  Op x <*> y = Op (fmap (<*> y) x)


instance Functor f => Monad (Free f) where
  Var k >>= f = f k
  Op z >>= f = Op $ fmap (>>= f) z

-- Nondeterminism
list :: Functor f => Free (Or :+: f) a -> Free f [a]
list = eval (embed alg) gen where
  gen x = Var [x]
  alg (Or mx my) = do xs <- mx
                      ys <- my
                      Var (xs ++ ys)


global :: Functor f => Free (Or :+: Stop :+: f) a -> Free f (Maybe [a])
global = fail' . list

local :: Functor f => Free (Stop :+: Or :+: f) a -> Free f [Maybe a]
local = list . fail'

main :: IO ()
main = print $ run d
