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

resistors = [1, 1.2, 1.5, 1.8, 2.2, 2.7, 3.3, 3.9, 4.7, 5.6, 6.8, 8.2]

iterateNetwork :: [Network] -> [Network]
iterateNetwork [] = [[Resistor r o] | r <- resistors, o <- ['f']]
iterateNetwork networks = [network ++ [Resistor r o] | network <- networks, r <- resistors, o <- ['s', 'p']]

flatten arr = [y | x<- arr, y <- x]

networks = flatten $ iterate (iterateNetwork) []

find :: Float -> Float -> [Resistor]
find target margin = [network | network <- networks, abs (equivalentResistance network - target) <= margin] !! 0

main = do
  putStrLn "Input a target resistance: "
  targetInput <- getLine
  let target = (read targetInput :: Float)
  putStrLn "Input an acceptible margin: "
  marginInput <- getLine
  let margin = (read marginInput :: Float)
  print (find target margin)
