import AufgabeFFP9

import Test.HUnit
import Test.QuickCheck

-- helpers

-- arbitrary types

-- properties

-- fixtures

-- tests

tests = TestList
  []

main = do
  quickCheck prop_coincide
  runTestTT tests
