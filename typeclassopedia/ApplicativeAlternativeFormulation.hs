class Functor f => Monoidal f where
  unit :: f ()
  (**) :: f a -> f b -> f (a, b)

-- a test instance
instance Monoidal Maybe where
  unit = Just ()
  Nothing ** _ = Nothing
  _ ** Nothing = Nothing
  Just x ** Just y = Just (x, y)

{-
    1. Implement pure and (<*>) in terms of unit and (**), and vice versa.
-}

class Monoidal f => Applicative f where
  pure :: a -> f a
  pure a = fmap (const a) unit

  (<*>) :: f (a -> b) -> f a -> f b
  f <*> a = fmap (\ (g, x) -> g x) (f Main.** a)

-- a test instance
instance Main.Applicative Maybe

class Prelude.Applicative f => Monoidal' f where
  unit' :: f ()
  unit' = Prelude.pure ()

  (**!) :: f a -> f b -> f (a, b)
  x **! y = (,) <$> x Prelude.<*> y

-- a test instance
instance Monoidal' Maybe

{-
    2. Are there any Applicative instances for which there are also functions
       f () -> () and f (a,b) -> (f a, f b), satisfying some "reasonable" laws?
-}

{-
    3. (Tricky) Prove that given your implementations from the first exercise,
       the usual Applicative laws and the Monoidal laws stated above are
       equivalent.
-}

