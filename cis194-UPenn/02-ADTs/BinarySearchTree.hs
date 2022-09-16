module MyBST where

data Tree = Leaf
           | Node Tree Int Tree
  deriving (Show, Eq)

insert :: Int -> Tree -> Tree
insert n Leaf = Node Leaf n Leaf
insert n node@(Node left k right)
  | n < k = Node (insert n left) k right
  | n > k = Node left k (insert n right)
  | otherwise = node

