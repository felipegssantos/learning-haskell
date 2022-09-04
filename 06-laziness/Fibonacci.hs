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
fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(x, y) -> (y, x + y)) (0, 1)

-- Exercise 3: define a stream
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : (streamToList xs)

instance Show a => Show (Stream a) where
  show stream = show $ take 10 (streamToList stream)

