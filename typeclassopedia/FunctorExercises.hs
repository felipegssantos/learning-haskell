{-
    1. Implement Functor instances for Either e and ((->) e).
-}

-- create custom Either and (->) types in order to avoid conflict
-- with built-in ones
data MyEither a b = L a | R b
data MyFunc a b = F (a -> b)

instance Functor (MyEither e) where
  fmap _ (L x) = L x
  fmap g (R x) = R (g x)

instance Functor (MyFunc e) where
  fmap g (F fa) = F (g . fa)

{-
    2. Implement Functor instances for ((,) e) and for Pair, defined as

         data Pair a = Pair a a

       Explain their similarities and differences.
-}

data MyTuple a b = T (a, b)
  deriving Show

data Pair a = Pair a a
  deriving Show

-- while Pair is a pair of values with same type, ((,) e) is a pair of
-- values whose first element is of type e and second one has any type

instance Functor (MyTuple e) where
  fmap g (T (e, x)) = T (e, g x)
  -- given g :: a -> b, this only makes sense with we apply g to the
  -- second argument of the tuple

instance Functor Pair where
  fmap g (Pair x y) = Pair (g x) (g y)
  -- given g :: a -> b, because both values in Pair are of the same type,
  -- it makes sense to apply g to both of them

{-
    3. Implement a Functor instance for the type ITree, defined as

         data ITree a = Leaf (Int -> a) 
                      | Node [ITree a]
-}

{-
    4. Give an example of a type of kind * -> * which cannot be made an instance of Functor (without using undefined).
-}

{-
    5. Is this statement true or false?

           The composition of two Functors is also a Functor.

       If false, give a counterexample; if true, prove it by exhibiting some appropriate Haskell code.
-}

