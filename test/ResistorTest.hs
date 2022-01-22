import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import ResistorBuilder (equivalentResistance, Resistor(..), find)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [canary, emptyResistance, singleResistance, simleSeriesResistance, simpleParallelResistance, mixedSeriesResistance, mixedParallelResistance, complexResistance, findUnit, findSimpleSeries]

canary =
  testCase "canary" $ assertEqual [] True (True)

emptyResistance = 
  testCase "resistance of an empty network" $ assertEqual [] 0 (equivalentResistance [])

singleResistance = 
  testCase "resistance of a single resistor" $ assertEqual [] 1 (equivalentResistance [Resistor 1 'f'])

simleSeriesResistance = 
  testCase "resistance of two unit resistors in series" $ assertEqual [] 2 (equivalentResistance [Resistor 1.0 'f', Resistor 1.0 's'])

simpleParallelResistance = 
  testCase "resistance of two unit resistors in parallel" $ assertEqual [] 0.5 (equivalentResistance [Resistor 1.0 'f', Resistor 1.0 'p'])

mixedSeriesResistance = 
  testCase "resistance of non-unit resistors in series" $ assertEqual [] 5 (equivalentResistance [Resistor 2.0 'f', Resistor 3.0 's'])

mixedParallelResistance = 
  testCase "resistance of non-unit resistors in parallel" $ assertEqual [] 0.9 (equivalentResistance [Resistor 1.0 'f', Resistor 9.0 'p'])

complexResistance = 
  testCase "resistance of a network using both series and parallel connections" $ assertEqual [] 0.6 (equivalentResistance [Resistor 1.0 'f', Resistor 1.0 'p', Resistor 1.0 's', Resistor 1.0 'p'])

findUnit = 
  testCase "find one resistor" $ assertEqual [] 1 (equivalentResistance (find 1.0 0.0))

findSimpleSeries = 
  testCase "find two units in series" $ assertEqual [] 2 (equivalentResistance (find 2.0 0.0))
