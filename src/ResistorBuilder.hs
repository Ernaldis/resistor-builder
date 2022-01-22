module ResistorBuilder ( Resistor(..)
                       , equivalentResistance
                       , find
                       ) where

data Resistor = Resistor { resistance :: Float, orientation :: Char } deriving (Show, Eq)
type Network = [Resistor]

equivalentResistance :: Network -> Float
equivalentResistance [] = 0
equivalentResistance [resistor] = resistance resistor
equivalentResistance [a, b]
  | orientation b == 's' = resistance a + resistance b
  | orientation b == 'p' = (resistance a * resistance b)/(resistance a + resistance b)
equivalentResistance (x:y:xs) = equivalentResistance ([Resistor rollingResistance 'f'] ++ xs)
  where rollingResistance = equivalentResistance [x, y]

resistors = [1, 1.2, 1.5, 1.8, 2.2, 2.7, 3.3, 3.9, 4.7, 5.6, 6.8, 8.2, 10, 12, 15, 18, 22, 27, 33, 39, 47, 56, 68, 82, 100, 120, 150, 180, 220, 270, 330, 390, 470, 560, 680, 820, 1000, 1200, 1500, 1800, 2200, 2700, 3300, 3900, 4700, 5600, 6800, 8200, 10000, 12000, 15000, 18000, 22000, 27000, 33000, 39000, 47000, 56000, 68000, 82000, 100000, 120000, 150000, 180000, 220000, 270000, 330000, 390000, 470000, 560000, 680000, 82000 , 1000000]

-- Greedy solution:
--  start with whichever resistor is closest to target
--  for each step, pick series or parallel as usual
--  pick resistance value to minimize difference between equivalent resistance, and target resistance

-- best candidate takes a list of networks and returns the one closest to the target
closestCandidate :: Float -> [Network] -> Network
closestCandidate target networks = foldl (\acc network -> if abs ((equivalentResistance acc) - target) < abs ((equivalentResistance network) - target) then acc else network) (head networks) networks

candidates :: Network -> [Network]
candidates [] = [[Resistor r 'f'] | r <- resistors]
candidates network = [network ++ [Resistor r o] | r <- resistors, o <- ['s', 'p']]

findNetwork :: Float -> Float -> Network -> Network
findNetwork target margin network
  | abs ((equivalentResistance network) - target) <= margin = network
  | otherwise = findNetwork target margin (closestCandidate target (candidates network))

find :: Float -> Float -> [Resistor]
find target margin = findNetwork target margin (closestCandidate target (candidates []))
