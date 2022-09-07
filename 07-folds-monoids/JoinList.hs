{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1: append joinlists using monoidal operation
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty jlist = jlist
(+++) jlist Empty = jlist
(+++) jlist1 jlist2 = Append (tag jlist1 <> tag jlist2) jlist1 jlist2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- Exercise 2.1: lookup JoinList by index
tagSize :: (Sized b, Monoid b) => JoinList b a -> Int
tagSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n (Append b left right)
  | n >= (getSize . size) b = Nothing
  | tagSize left <= n = indexJ (n - tagSize left) right
  | otherwise = indexJ n left
indexJ n (Single _ a)
  | n == 0 = Just a -- Single's have a single index: zero
  | otherwise = Nothing

