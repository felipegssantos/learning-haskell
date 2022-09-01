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

