import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Resistor (addTwo, equivalentResistance)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [canary, testAddTwo, emptyResistance, singleResistance, simleSeriesResistance, simpleParallelResistance, mixedSeriesResistance, mixedParallelResistance, complexResistance ]

canary =
  testCase "canary" $ assertEqual [] True (True)

testAddTwo =
  testCase "add Two" $ assertEqual [] 3 (addTwo 1 2)

emptyResistance = 
  testCase "resistance of an empty network" $ assertEqual [] 0 (equivalentResistance [])

singleResistance = 
  testCase "resistance of a single resistor" $ assertEqual [] 1 (equivalentResistance [(1, 'f')])

simleSeriesResistance = 
  testCase "resistance of two unit resistors in series" $ assertEqual [] 2 (equivalentResistance [(1, 'f'), (1, 's')])

simpleParallelResistance = 
  testCase "resistance of two unit resistors in parallel" $ assertEqual [] 0.5 (equivalentResistance [(1, 'f'), (1, 'p')])

mixedSeriesResistance = 
  testCase "resistance of non-unit resistors in series" $ assertEqual [] 5 (equivalentResistance [(2, 'f'), (3, 's')])

mixedParallelResistance = 
  testCase "resistance of non-unit resistors in parallel" $ assertEqual [] 0.9 (equivalentResistance [(1, 'f'), (9, 'p')])

complexResistance = 
  testCase "resistance of a network using both series and parallel connections" $ assertEqual [] 0.6 (equivalentResistance [(1, 'f'), (1, 'p'), (1, 's'), (1, 'p')])
