type Peg = String
type Move = (String, String)

-- Solve tower of Hanoi, move tower from peg1 to peg2
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n peg1 peg2 peg3
  | n == 1 = [(peg1, peg2)]
  | n > 1 = (hanoi (n-1) peg1 peg3 peg2)
            ++ [(peg1, peg2)]
            ++ (hanoi (n-1) peg3 peg2 peg1)
  | otherwise = []

-- Solve for 4 towers of Hanoi, move tower from peg1 to peg2
-- Follows Chu & Johnsonbaugh, "The Four-Peg Tower of Hanoi Puzzle",
-- SIGCSE Bulletin 23, p. 2 (1991).
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n peg1 peg2 peg3 peg4
  | n > 1 = let k = (chooseKay n) in (hanoi4 (n-k) peg1 peg4 peg2 peg3)
            ++ (hanoi k peg1 peg2 peg3)
            ++ (hanoi4 (n-k) peg4 peg2 peg3 peg1)
  | n == 1 = [(peg1, peg2)]
  | otherwise = []

chooseKay :: Integer -> Integer
chooseKay n = maximum [k | k <- [1..(n-1)], k*(k+1) `div` 2 <= n]

