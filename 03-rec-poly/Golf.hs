{-# OPTIONS_GHC -Wall #-}
module Golf where

skips :: [a] -> [[a]]
skips list = map (getEveryNth list) [1..(length list)]

-- takes in a list and returns a function which takes an Int, n, and returns
-- a second list containing every nth element of the first list
getEveryNth :: [a] -> Int -> [a]
getEveryNth list n = (fst . unzip) (getEveryNthFromIndexedList list n)

getEveryNthFromIndexedList :: [a] -> Int -> [(a, Int)]
getEveryNthFromIndexedList list n
  = filter (\(_, i) -> i `mod` n == 0) (zip list [1..(length list)])

