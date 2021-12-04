import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Resistor (addTwo)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [canary, testAddTwo ]

canary =
  testCase "canary" $ assertEqual [] True (True)

testAddTwo =
  testCase "add Two" $ assertEqual [] 3 (addTwo 1 2)
