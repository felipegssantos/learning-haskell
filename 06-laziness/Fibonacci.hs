{-# LANGUAGE FlexibleInstances #-}
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
  show stream = show $ take 20 (streamToList stream)

-- Exercise 4: stream generation and mapping
streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))

-- Exercise 5: build non-trivial streams
{-
Implementing nats is straightforward, but ruler is tricky.

In order to avoid more and more divisions by two as the stream grows, my
approach is to use binary. For all odd numbers, the largest power of 2 that
divides the number evenly is trivially 0. For even numbers, we look at the
least significant binary digit which equals 1. For example,

decimal | binary | max power of 2 dividing evenly
      2 |     10 | 1
      4 |    100 | 2
      6 |    110 | 1
      8 |   1000 | 3
     10 |   1010 | 1
     12 |   1100 | 2
     14 |   1110 | 1
     16 |  10000 | 4

The maximum power of 2 divider is the number of zeros before the least
significant one. We can get the maximum power of 2 divider for even numbers
as follows:
1. Build a stream of binary numbers from 1 (1, 10, 11, 100...). These will
   be the binary representation of even numbers, discarding the last zero.
2. For each number of the binary stream, find the least significant "1" and
   count the number of zeros to the right of it. This number plus one is the
   maximum power of 2 dividing the corresponding even number.
-}
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) powerOfTwoDivider

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) (Stream y ys)
  = Stream x $ Stream y $ interleaveStreams xs ys

-- stream of power 2 dividing even numbers
powerOfTwoDivider :: Stream Integer
powerOfTwoDivider = streamMap (fromIntegral . maxDivider) binaries
  where maxDivider bin = 1 + (length $ takeWhile (==Zero) bin)

-- stream of binaries
binaries :: Stream Binary
binaries = streamFromSeed nextBinary [One]

-- for convenience, binaries are represented in reverse order
-- (most significant digit LAST, least significant FIRST)
nextBinary :: Binary -> Binary
nextBinary [] = [One]
nextBinary (Zero:bs) = One : bs
nextBinary (One:bs) = Zero : (nextBinary bs)

data ZeroOrOne = Zero | One
  deriving (Show, Eq)

type Binary = [ZeroOrOne]

-- Exercise 6: Fibonacci with generating functions
x :: Stream Integer
x = Stream 0 (Stream 1 $ streamRepeat 0)

instance Num (Stream Integer) where
  fromInteger n = Stream n (streamRepeat 0)
  negate (Stream a0 a') = Stream (-a0) (negate a')
  (+) (Stream a0 a') (Stream b0 b') = Stream (a0 + b0) (a' + b')
  (*) a@(Stream a0 a') b@(Stream b0 b')
    = Stream (a0 * b0) ((streamMap (*a0) b') + a' * b)

instance Fractional (Stream Integer) where
  (/) a@(Stream a0 a') b@(Stream b0 b')
    = Stream (a0 `div` b0) $ streamMap (`div` b0) (a' - (a / b) * b')

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7: Fibonacci numbers via matrices
data Matrix = Matrix Integer Integer Integer Integer
  deriving Show

instance Num Matrix where
  (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22)
    = Matrix (a11 * b11 + a12 * b21) (a11 * b12 + a12 * b22)
             (a21 * b11 + a22 * b21) (a21 * b12 + a22 * b22)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = projectOut $ (Matrix 1 1 1 0)^n
  where projectOut (Matrix _ _ a21 _) = a21

