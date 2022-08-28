{-# OPTIONS_GHC -Wall #-}
module Golf where

{-
Exercise 1: Hopscotch

My approach relies on mapping every index, n, in [1..(length list)] to the
corresponding list which takes every nth from the input list. I build the
mapping function by currying a function that takes a list and an integer
(in this order) and returns a list with every nth element of the input list.

The actual job of taking every nth element of a list is done by a third
function that takes in an "indexed list", i.e., a list zipped with
[1..length(list)], and filters in every nth element by looking into this
auxiliary index.
-} 

-- returns a list of the same size of the input list where the nth element
-- is itself a list containing every nth element of the input list
skips :: [a] -> [[a]]
skips list = map (getEveryNth list) [1..(length list)]

-- takes in a list and an integer and returns a new list containing every
-- nth element of the input list
getEveryNth :: [a] -> Int -> [a]
getEveryNth list n = (fst . unzip) (getEveryNthFromIndexedList list n)

-- auxiliary function that takes in a list, indexes it and uses this index
-- to filter in every nth element of the input list
getEveryNthFromIndexedList :: [a] -> Int -> [(a, Int)]
getEveryNthFromIndexedList list n
  = filter (\(_, i) -> i `mod` n == 0) (zip list [1..(length list)])


{-
Exercise 2: Local maxima
-}

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  | y > (maximum [x, z]) = y : (localMaxima (z:xs))
  | otherwise = localMaxima (y:z:xs)
localMaxima _ = []


{-
Exercise 3: Histogram
-}

-- TODO: make transpose safe by means of the type system

-- transpose a list of lists
transpose :: a -> [[a]] -> [[a]]
transpose _ ([]:_) = [] -- avoids that the default value def is ever used
transpose def xs = (map (headDef def) xs) : (transpose def (map tailSafe xs))

-- safe way to get the head of a list and returning a default value in case
-- the list is empty
headDef :: a -> [a] -> a
headDef def [] = def -- does not really matter; transpose never reaches it
headDef _ (x:_) = x

-- safe way to get the tail of list, defaulting to an empty list in case
-- the input is empty
tailSafe :: [a] -> [a] 
tailSafe [] = [] -- does not really matter; transpose never reaches it
tailSafe (_:xs) = xs

histogram :: [Integer] -> String
histogram digits = let xAxis = [replicate 10 '=', "0123456789"]
                       histBars = countsToStrings . countDigits
                   in unlines $ (transpose '?' (histBars digits))
                                ++ xAxis

countsToStrings :: [Integer] -> [String]
countsToStrings counts
  = let size = maximumSafe counts
    in map (\count -> 
             (replicate (fromIntegral (size - count)) ' ')
             ++ (replicate (fromIntegral count) '*'))
           counts

-- a safe way to get the maximum among (non-negative) integers
maximumSafe :: [Integer] -> Integer
maximumSafe [] = 0
maximumSafe xs = maximum xs

-- counts digits in a list
countDigits :: [Integer] -> [Integer]
countDigits digits = foldl countOneDigit (replicate 10 0) digits

countOneDigit :: [Integer] -> Integer -> [Integer]
countOneDigit currentCounts n
  = map (\(count, i) -> if i == n then count + 1 else count)
        (zip currentCounts [0..9])

