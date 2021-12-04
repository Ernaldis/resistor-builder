module Resistor where

addTwo a b = a + b

equivalentResistance :: [(Float, Char)] -> Float
equivalentResistance [] = 0
equivalentResistance [(resistance, _)] = resistance
equivalentResistance [(a, _), (b, x)]
  | x == 's' = a + b
  | x == 'p' = (a*b)/(a+b)
equivalentResistance (x:y:xs) = equivalentResistance ([(rollingResistance, 'f')] ++ xs)
  where rollingResistance = equivalentResistance [x, y]
