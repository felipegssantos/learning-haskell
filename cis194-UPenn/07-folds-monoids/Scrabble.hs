{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module Scrabble where

import Data.Char

-- Exercise 3: score using Scrabble scoring system
newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = Score 0

score :: Char -> Score
score char
  | (toUpper char) `elem` "AEILNORSTU" = Score 1
  | (toUpper char) `elem` "DG" = Score 2
  | (toUpper char) `elem` "BCMP" = Score 3
  | (toUpper char) `elem` "FHVWY" = Score 4
  | (toUpper char) `elem` "K" = Score 5
  | (toUpper char) `elem` "JX" = Score 8
  | (toUpper char) `elem` "QZ" = Score 10
  | otherwise = 0

scoreString :: String -> Score
scoreString = sum . (map score)

