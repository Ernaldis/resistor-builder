import           ResistorBuilder (Resistor(..), equivalentResistance, find)
import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [canary, emptyResistance, singleResistance, mixedSeriesResistance, mixedParallelResistance, complexResistance, findUnit, findSimpleSeries]

canary =
  testCase "canary" $ assertEqual [] True (True)

emptyResistance =
  testCase "resistance of an empty network" $ assertEqual [] (Nothing) (equivalentResistance [])

singleResistance =
  testCase "resistance of a single resistor" $ assertEqual [] (Just 1) (equivalentResistance [Resistor 1 'f'])

mixedSeriesResistance =
  testCase "resistance of two resistors in series" $ assertEqual [] (Just 5) (equivalentResistance [Resistor 2.0 'f', Resistor 3.0 's'])

mixedParallelResistance =
  testCase "resistance of two resistors in parallel" $ assertEqual [] (Just 0.9) (equivalentResistance [Resistor 1.0 'f', Resistor 9.0 'p'])

complexResistance =
  testCase "resistance of a network using both series and parallel connections" $ assertEqual [] (Just 0.6) (equivalentResistance [Resistor 1.0 'f', Resistor 1.0 'p', Resistor 1.0 's', Resistor 1.0 'p'])

findUnit =
  testCase "find one resistor" $ assertEqual [] (Just [Resistor 1 'f']) (find 1 0)

findSimpleSeries =
  testCase "find two units in series" $ assertEqual [] (Just [Resistor 1 'f', Resistor 1 's']) (find 2 0)
