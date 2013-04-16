{-# LANGUAGE TemplateHaskell #-}

import Prelude hiding (filter)
import Test.HUnit

import AufgabeFFP3


-- part 1: knapsack problem

testsel1no1 = TestCase $ assertEqual ""
  ((selector1 . (filter 5) . transformer . generator) [(5,3),(2,7),(2,6),(10,100)])
  [([(2,6),(2,7)],4,13)]

testsel1no2 = TestCase $ assertEqual ""
  ((selector1 . (filter 13) . transformer . generator) [(5,3),(2,7),(2,6),(10,100)])
  [([(10,100),(2,7)],12,107)]

testsel1no3 = TestCase $ assertEqual ""
  ((selector1 . (filter 1) . transformer . generator) [(5,3),(2,7),(2,6),(10,100)])
  []

testsel1no4 = TestCase $ assertEqual ""
  ((selector1 . (filter 5) . transformer . generator) [(5,13),(2,7),(2,6),(10,100)])
  [([(2,6),(2,7)],4,13),([(5,13)],5,13)]

testsel2no1 = TestCase $ assertEqual ""
  ((selector2 . (filter 5) . transformer . generator) [(5,13),(2,7),(2,6),(10,100)])
  [([(2,6),(2,7)],4,13)]

-- part 2: binomial coefficient


tests = TestList
  [ TestLabel "Test selection 1 no. 1" testsel1no1
  , TestLabel "Test selection 1 no. 2" testsel1no2
  , TestLabel "Test selection 1 no. 3" testsel1no3
  , TestLabel "Test selection 1 no. 4" testsel1no4
  , TestLabel "Test selection 2 no. 1" testsel2no1
  ]

main = do
  runTestTT tests

-- vim:sts=2:sw=2
