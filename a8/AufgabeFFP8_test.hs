{-# LANGUAGE TemplateHaskell #-}

-- This test file requires GHC.
-- WARNING: check manually that the code works with Hugs too.

import AufgabeFFP8

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.All

-- properties

-- quickCheckAll generates test cases for all 'prop_*' properties

-- fixtures

-- tests

tests = TestList
  [
--"test name" ~: expected_result ~=? actual_result
  ]

main = do
  $(quickCheckAll)
  runTestTT tests
