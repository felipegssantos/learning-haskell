{-
1. Implement a function

     sequenceAL :: Applicative f => [f a] -> f [a]

   There is a generalized version of this, sequenceA, which works for any
   Traversable (see the later section on Traversable), but implementing this
   version specialized to lists is a good exercise.
-}

import Control.Applicative

sequenceAL :: Applicative f => [f a] -> f [a]
sequenceAL [] = pure []
sequenceAL (x:xs) = liftA2 (:) x (sequenceAL xs)

