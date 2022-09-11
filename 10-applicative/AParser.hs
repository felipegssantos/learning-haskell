{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1: build a functor instance for Parser
instance Functor Parser where
  fmap f pa = Parser go
    where
      go input = (applyFirst f) <$> runParser pa input

applyFirst :: (a -> b) -> (a, c) -> (b, c)
applyFirst f (a, c) = (f a, c)

-- Exercise 2: build an applicative instance for Parser
instance Applicative Parser where
  pure a = Parser (\str -> Just (a, str))
  pf <*> pa = Parser go
    where
      go input = case (func, value) of
        (Just (f, _), Just (a, rest)) -> Just (f a, rest)
        _ -> Nothing
        where
          func = runParser pf input
          value = case func of
            Just (_, rest) -> runParser pa rest
            _ -> Nothing

-- Exercise 3: testing the applicative instance
abParser :: Parser (Char, Char)
abParser = (\a b -> (a, b)) <$> (char 'a') <*> (char 'b')

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> (char 'a') <*> (char 'b')

intPair :: Parser [Integer]
intPair = (\x y -> [x, y]) <$> parseFirst <*> posInt
  where parseFirst = (\x _ -> x) <$> posInt <*> char ' '

