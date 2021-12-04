module ResistorBuilder ( Resistor(..)
                       , equivalentResistance
                       ) where

data Resistor = Resistor { resistance :: Float, orientation :: Char }

equivalentResistance :: [Resistor] -> Float
equivalentResistance [] = 0
equivalentResistance [resistor] = resistance resistor
equivalentResistance [a, b]
  | orientation b == 's' = resistance a + resistance b
  | orientation b == 'p' = (resistance a * resistance b)/(resistance a + resistance b)
equivalentResistance (x:y:xs) = equivalentResistance ([Resistor rollingResistance 'f'] ++ xs)
  where rollingResistance = equivalentResistance [x, y]

