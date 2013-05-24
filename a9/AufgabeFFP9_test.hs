import AufgabeFFP9

import Test.HUnit
import Test.QuickCheck

-- helpers

-- arbitrary types

-- properties

-- fixtures

-- tests

tests = TestList
  ["occS 1" ~: [(0,2), (2,4)] ~=? occS "aaaa" "aa"
  ,"occS 2" ~: [(1,3)] ~=? occS "abab" "ba"
  ]

main = do
  quickCheck prop_coincide
  runTestTT tests
