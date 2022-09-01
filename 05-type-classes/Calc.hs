{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser

-- Exercise 1: evaluate expressions
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add n m) = (eval n) + (eval m)
eval (Mul n m) = (eval n) * (eval m)

-- Exercise 2: parse and evaluate
evalStr :: String -> Maybe Integer
evalStr = evalIfJust . (parseExp Lit Add Mul)

evalIfJust :: Maybe ExprT -> Maybe Integer
evalIfJust = maybe Nothing (\x -> Just (eval x))

-- Exercise 3: abstract with type class
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- Exercise 4: instances of type class
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 n) (Mod7 m) = lit (n + m)
  mul (Mod7 n) (Mod7 m) = lit (n * m)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

