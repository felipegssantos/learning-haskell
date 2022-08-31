{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT

-- Exercise 1: evaluate expressions
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add n m) = (eval n) + (eval m)
eval (Mul n m) = (eval n) * (eval m)


