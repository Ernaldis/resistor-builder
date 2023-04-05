module ResistorBuilder ( Resistor(..)
                       , equivalentResistance
                       , find
                       ) where

import           Data.Maybe (fromMaybe)

data Resistor = Resistor { resistance :: Float, orientation :: Char } deriving (Show, Eq)
type Network = [Resistor]

resistors :: [Float]
resistors =[1, 1.2, 1.5, 1.8, 2.2, 2.7, 3.3, 3.9, 4.7, 5.6, 6.8, 8.2] >>= (\r -> map (*r) [10^x | x <- [0..5]])

equivalentResistance :: Network -> Maybe Float
equivalentResistance [] = Nothing
equivalentResistance [resistor] = Just (resistance resistor)
equivalentResistance (r:rs) =
  equivalentResistance rs >>= \remaining ->
  case orientation (head rs) of
    's' -> Just (resistance r + remaining)
    'p' -> Just ((resistance r * remaining)/(resistance r + remaining))

candidates :: Network -> [Network]
candidates [] = [[Resistor r 'f'] | r <- resistors]
candidates network = [network ++ [Resistor r o] | r <- resistors, o <- ['s', 'p']]

allNetworks :: [Network]
allNetworks = concat $ iterate (>>= candidates) (candidates [])

isGoodNetwork :: Network -> Float -> Float -> Bool
isGoodNetwork network target margin =
  case equivalentResistance network of
    Just r  -> abs(r - target) <= margin
    Nothing -> False

find :: Float -> Float -> Maybe Network
find 0 _ = Nothing
find target margin = Just . head $ filter (\n -> isGoodNetwork n target margin) allNetworks
