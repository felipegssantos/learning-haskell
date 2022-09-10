{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Tree

-- Exercise 1
-- 1.1: add an employee to the guest list and update the fun
-- (no checks are made)
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL guests fun) = GL (emp:guests) (fun + empFun emp)

-- 1.2: implement monoid structure for GuestList
instance Semigroup GuestList where
  (GL guests1 fun1) <> (GL guests2 fun2)
    = GL (guests1 ++ guests2) (fun1 + fun2)

instance Monoid GuestList where
  mempty = GL [] 0

-- 1.3: compare fun of guest lists
moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2
  | gl1 > gl2 = gl1
  | otherwise = gl2

-- Exercise 2: fold trees
foldTree' :: (a -> [b] -> b) -> Tree a -> b
foldTree' f (Node a []) = f a []
foldTree' f (Node a trees) = f a (map (foldTree' f) trees)

-- Exercise 3: guest lists with or without bosses
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp [] = (glCons emp mempty, mempty)
nextLevel boss guestLists
  = (
      foldr (<>) (glCons boss mempty) (map snd guestLists),
      foldr (<>) mempty (map (uncurry moreFun) guestLists)
    )

-- Exercise  4: maximize overall fun
maxFun :: Tree Employee -> GuestList
maxFun tree = (uncurry moreFun) (buildGuestLists tree)

buildGuestLists :: Tree Employee -> (GuestList, GuestList)
buildGuestLists (Node emp []) = nextLevel emp []
buildGuestLists (Node boss trees) = nextLevel boss (map buildGuestLists trees)

