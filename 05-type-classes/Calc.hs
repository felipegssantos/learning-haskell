{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser
import StackVM

-- Exercise 1: evaluate expressions
eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add n m) = (eval n) + (eval m)
eval (ExprT.Mul n m) = (eval n) * (eval m)

-- Exercise 2: parse and evaluate
evalStr :: String -> Maybe Integer
evalStr = evalIfJust . (parseExp ExprT.Lit ExprT.Add ExprT.Mul)

evalIfJust :: Maybe ExprT -> Maybe Integer
evalIfJust = maybe Nothing (\x -> Just (eval x))

-- Exercise 3: abstract with type class
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

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

-- Exercise 5: compiler for arithmetic expressions
instance Expr Program where
  lit n = [PushI n]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

