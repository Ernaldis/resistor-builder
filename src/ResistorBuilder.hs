module ResistorBuilder ( Resistor(..)
                       , equivalentResistance
                       , find
                       ) where

data Resistor = Resistor { resistance :: Float, orientation :: Char } deriving (Show, Eq)

equivalentResistance :: [Resistor] -> Float
equivalentResistance [] = 0
equivalentResistance [resistor] = resistance resistor
equivalentResistance [a, b]
  | orientation b == 's' = resistance a + resistance b
  | orientation b == 'p' = (resistance a * resistance b)/(resistance a + resistance b)
equivalentResistance (x:y:xs) = equivalentResistance ([Resistor rollingResistance 'f'] ++ xs)
  where rollingResistance = equivalentResistance [x, y]

-- Greedy solution:
--  start with whichever resistor is closest to target
--  for each step, pick series or parallel as usual
--  pick resistance value to minimize difference between equivalent resistance, and target resistance

findNetwork :: Float -> Float -> [Resistor] -> [Resistor]
findNetwork target margin network
  | abs (resistance - target) <= margin = network
  | remainingSeries > 0 = find remainingSeries margin ++ [Resistor r1 's'] ++ tail network
  | remainingParallel > 0 = find remainingParallel margin ++ [Resistor r1 'p'] ++ tail network
  where resistance = equivalentResistance network
        remainingSeries = target-resistance
        remainingParallel = (target * resistance)/(resistance - target)
        r1 = ohms $ head network

find :: Float -> Float -> [Resistor]
find target margin = findNetwork target margin [Resistor 1 'f']

ohms :: Resistor -> Float
ohms resistor = resistance resistor
