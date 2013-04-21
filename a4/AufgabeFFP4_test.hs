{-# LANGUAGE TemplateHaskell #-}

import Prelude hiding (filter)
import Test.HUnit

import AufgabeFFP4

testknp1 = TestCase $ assertEqual ""
  ([(3,4),(3,4),(2,3),(2,3)],14)
  (knapsack [(2,3),(2,3),(3,4),(3,4),(5,6)] 10)

testbin1 = TestCase $ assertEqual ""
  6 (binomDyn (4,2))

tests = TestList
  [ TestLabel "knapsack 1" testknp1
  , TestLabel "binomial 1" testbin1
  ]

main = do
  runTestTT tests

-- vim:sts=2:sw=2
