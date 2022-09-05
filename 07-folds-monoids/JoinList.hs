{-# OPTIONS_GHC -Wall #-}

module JoinList where

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1: append joinlists using monoidal operation
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jlist1 jlist2 = Append (tag jlist1 <> tag jlist2) jlist1 jlist2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

