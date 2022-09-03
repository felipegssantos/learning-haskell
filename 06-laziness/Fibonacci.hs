{-# OPTIONS_GHC -Wall #-}

module Fibonacci where

-- Exercise 1: naive fibonacci
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2: refactored fibonacci

