{-# OPTIONS_GHC -Wall #-}

{-
Exercise 1: Wholemeal programming
-}

-- 1.1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (2-) . filter even

-- 1.2
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate next

next :: Integer -> Integer
next n = if (even n) then (n `div` 2) else (3 * n + 1)

{-
Exercise 2: Folding with trees
-}

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- inserts new element to a tree by attaching the new element to the shorter
-- subtree of the root and then updating heights if necessary
insert :: a -> Tree a -> Tree a
insert new Leaf = Node 0 Leaf new Leaf
insert new (Node h left old right)
  | height left < height right = Node h (insert new left) old right
  | height left > height right = Node h left old (insert new right)
  | isFull left = Node (h + 1) (insert new left) old right
  | otherwise = Node h (insert new left) old right

height :: Tree a -> Integer
height Leaf = -1 -- make sure leaves have height smaller than any node
height (Node h _ _ _) = h

isFull :: Tree a -> Bool
isFull Leaf = True
isFull (Node _ left _ right) = (height left == height right)
                               && isFull left && isFull right

