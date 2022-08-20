type Peg = String
type Move = (String, String)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n peg1 peg2 peg3
  | n == 1 = [(peg1, peg2)]
  | n > 1 = (hanoi (n-1) peg1 peg3 peg2)
            ++ [(peg1, peg2)]
            ++ (hanoi (n-1) peg3 peg2 peg1)
  | otherwise = []

