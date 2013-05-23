{-# LANGUAGE TemplateHaskell #-}

-- This test file requires GHC.
-- WARNING: check manually that the code works with Hugs too.

import AufgabeFFP9

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.All

-- helpers

-- arbitrary types

-- properties

-- fixtures

-- tests

tests = TestList
  []

main = do
  $(quickCheckAll)
  runTestTT tests
