{-# OPTIONS_GHC -Wall #-}
-- Implementing exercises from the end of lecture 11

import Control.Applicative

(*>) :: Applicative f => f a -> f b -> f b
fa *> fb = seq fa fb

{-
Since:
foldr :: (a -> b -> b) -> b -> [a] -> b

One can, under suitable arguments, have:
foldr :: (a -> f [b] -> f [b]) -> f [b] -> [a] -> f [b]

If we can find a function of type a -> f [b] -> f [b] and a suitable
initial value of type f [b], the problem is solved.

For the initial value of type f [b], one can trivially take pure []

The function a -> f [b] -> f [b] can be built by taking into account
that the input function (call it g) has type a -> f [b]. If we now note
that liftA2 :: (a -> b -> c) -> f a -> f b -> f c, we can choose its
arguments such that liftA2 x ys :: a -> f [b] -> f [b]. The most
trivial way would be to choose the function
trivial_function x ys = ys

But we would like something more elaborate, probably using the list
constructor, (:). Since (:) :: b -> [b] -> [b], we can lift it in order
to reach the desired result:
cons_g x ys = liftA2 (:) (g x) ys
-}
mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA g = foldr cons_g (pure [])
  where cons_g x ys = liftA2 (:) (g x) ys

-- sequenceA :: Applicative f => [f a] -> f [a]

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n fa = (replicate n) <$> fa

