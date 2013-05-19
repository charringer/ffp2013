{-# LANGUAGE TemplateHaskell #-}

-- This test file requires GHC.
-- WARNING: check manually that the code works with Hugs too.

import AufgabeFFP8

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.All

-- helpers

eval :: Expr -> Integer
eval (Opd d) = d
eval (Opr P a b) = (eval a) + (eval b)
eval (Opr T a b) = (eval a) * (eval b)

-- arbitrary types

newtype Problem = Problem (Digits, TargetValue)
  deriving Show

instance Arbitrary Problem where
  arbitrary = do
    i <- choose (2,9) :: Gen Int
    tv <- choose (1, 200) :: Gen Integer
    return (Problem (dgts i, tv))
      where
        dgts i = take i digits

-- properties

prop_resultsEvalWell (Problem (dgts, tv)) = all evalWell results
  where
    results = mkTV dgts tv
    evalWell expr = (eval expr) == tv

-- fixtures

tv100expr1 = (Opr P (Opr P (Opr P (Opr P (Opr P (Opd 12) (Opd 34)) (Opr T (Opd 5) (Opd 6))) (Opd 8)) (Opd 7)) (Opd 9))
tv100expr2 = (Opr P (Opr P (Opr P (Opr P (Opr P (Opr P (Opd 1) (Opr T (Opd 2) (Opd 3))) (Opd 4)) (Opd 5)) (Opd 67)) (Opd 8)) (Opd 9))

-- tests

tests = TestList
  ["internal eval test 1" ~: 100 ~=? eval tv100expr1
  ,"internal eval test 2" ~: 100 ~=? eval tv100expr2

  ]

main = do
  $(quickCheckAll)
  runTestTT tests
