{-
    1. Implement an instance of Applicative for Maybe.
-}

data Maybe' a = Nothing'
              | Just' a
  deriving Show

instance Functor Maybe' where
  fmap f Nothing' = Nothing'
  fmap f (Just' x) = Just' (f x)

instance Applicative Maybe' where
  pure = Just'
  Nothing' <*> _ = Nothing'
  _ <*> Nothing' = Nothing'
  Just' f <*> Just' x = Just' (f x)

{-
    2. Determine the correct definition of pure for the ZipList instance of
       Applicative — there is only one implementation that satisfies the law
       relating pure and (<*>).
-}

newtype ZipList' a = ZipList' [a]

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' (map f xs)

instance Applicative ZipList' where
  (ZipList' gs) <*> (ZipList' xs) = ZipList' (zipWith ($) gs xs)
  pure = ZipList' . repeat

