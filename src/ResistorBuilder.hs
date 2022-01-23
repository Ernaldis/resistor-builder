module ResistorBuilder ( Resistor(..)
                       , equivalentResistance
                       , find
                       ) where

import System.Environment

data Resistor = Resistor { resistance :: Float, orientation :: Char } deriving (Show, Eq)
type Network = [Resistor]

equivalentResistance :: Network -> Float
equivalentResistance [resistor] = resistance resistor
equivalentResistance (r:rs)
  | orientation (head rs) == 's' = resistance r + remaining
  | orientation (head rs) == 'p' = (resistance r * remaining)/(resistance r + remaining)
  where remaining = equivalentResistance rs

resistors = [1, 1.2, 1.5, 1.8, 2.2, 2.7, 3.3, 3.9, 4.7, 5.6, 6.8, 8.2, 10, 12, 15, 18, 22, 27, 33, 39, 47, 56, 68, 82, 100, 120, 150, 180, 220, 270, 330, 390, 470, 560, 680, 820, 1000, 1200, 1500, 1800, 2200, 2700, 3300, 3900, 4700, 5600, 6800, 8200, 10000, 12000, 15000, 18000, 22000, 27000, 33000, 39000, 47000, 56000, 68000, 82000, 100000, 120000, 150000, 180000, 220000, 270000, 330000, 390000, 470000, 560000, 680000, 820000 , 1000000]

iterateNetwork :: [Network] -> [Network]
iterateNetwork [] = [[Resistor r o] | r <- resistors, o <- ['f']]
iterateNetwork networks = [network ++ [Resistor r o] | network <- networks, r <- resistors, o <- ['p', 's']]

flatten :: [[a]] -> [a]
flatten arr = [y | x<- arr, y <- x]

networks = flatten $ iterate (iterateNetwork) []

find :: Float -> Float -> [Resistor]
find target margin = [network | network <- networks, abs (equivalentResistance network - target) <= margin] !! 0

main = do
  args <- getArgs
  let targetInput = args !! 0
  let target = (read targetInput :: Float)
  let result = find target 0

  putStr "Target:\n"
  print target
  putStr "Result:\n"
  print [(resistance resistor, orientation resistor) | resistor <- result]
