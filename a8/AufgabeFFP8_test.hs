{-# LANGUAGE TemplateHaskell #-}

-- This test file requires GHC.
-- WARNING: check manually that the code works with Hugs too.

import AufgabeFFP8

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.All

import Data.Char

-- helpers

eval :: Expr -> Integer
eval (Opd d) = d
eval (Opr P a b) = (eval a) + (eval b)
eval (Opr T a b) = (eval a) * (eval b)

extractDigits :: Expr -> Digits
extractDigits (Opd n) =
  map c2i (show n)
    where
      c2i c = toInteger $ (ord c) - (ord '0')
extractDigits (Opr _ a b) =
  (extractDigits a) ++ (extractDigits b)

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

prop_resultsKeepDigits (Problem (dgts, tv)) = all keepDigits results
  where
    results = mkTV dgts tv
    keepDigits expr = (extractDigits expr) == dgts

-- fixtures

tv100expr1 = (Opr P (Opr P (Opr P (Opr P (Opr P (Opd 12) (Opd 34)) (Opr T (Opd 5) (Opd 6))) (Opd 7)) (Opd 8)) (Opd 9))
tv100expr2 = (Opr P (Opr P (Opr P (Opr P (Opr P (Opr P (Opd 1) (Opr T (Opd 2) (Opd 3))) (Opd 4)) (Opd 5)) (Opd 67)) (Opd 8)) (Opd 9))

-- tests

tests = TestList
  ["internal eval test 1" ~: 100 ~=? eval tv100expr1
  ,"internal eval test 2" ~: 100 ~=? eval tv100expr2
  ,"internal extractDigits test 1" ~: digits ~=? extractDigits tv100expr1
  ,"internal extractDigits test 2" ~: digits ~=? extractDigits tv100expr2
  ,"MaC has >= 2 solutions"   ~: True ~=? length ((mkTV digits 100)) >= 2
  --example A: 400 == 123+45*6+7
  ,"example A has a solution" ~: True ~=? length (mkTV [1..7] 400)   >= 1
  ,"min.value of 1..9"        ~: True ~=? length (mkTV [1..9] 45)    >= 1
  ,"min.value of 1..9 -1"     ~: True ~=? length (mkTV [1..9] 44)    == 0
  ]

main = do
  $(quickCheckAll)
  runTestTT tests
