{-
    1. Although it is not possible for a Functor instance to satisfy the
       first Functor law but not the second (excluding undefined), the
       reverse is possible. Give an example of a (bogus) Functor instance
       which satisfies the second law but not the first.
-}

newtype EvilList a = EL [a]
  deriving Show

instance Functor EvilList where
  fmap _ = const (EL [])

-- fmap id == const (EL []), violating the first law
-- fmap (f . g) == (fmap f) . (fmap g) == const (EL []) and satisfies
-- the second law

{-
    instance Functor [] where
      fmap :: (a -> b) -> [a] -> [b]
      fmap _ [] = []
      fmap g (x:xs) = g x : g x : fmap g xs

    2. Which laws are violated by the evil Functor instance for list shown
       above: both laws, or the first law alone? Give specific counterexamples.
-}

-- fmap id [x] == [x, x] (violates first law)
-- fmap (f . g) [x] == [f (g x), f (g x)]
-- (fmap f) (fmap g [x]) == fmap f [g x, g x]
--                       == f (g x) : f (g x) : fmap f [g x]
--                       (violates second law)                            

