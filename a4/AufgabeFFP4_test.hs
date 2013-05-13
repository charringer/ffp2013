{-# LANGUAGE TemplateHaskell #-}

import Prelude hiding (filter)
import Test.HUnit

import AufgabeFFP4

testknp1 =
  ([(3,4),(3,4),(2,3),(2,3)],14) ~=?
  (knapsack [(2,3),(2,3),(3,4),(3,4),(5,6)] 10)

testknpgrader1 = ([(2,3)],3) ~=? knapsack [(2,3)] 3

testknpgrader2 = ([(2,3),(2,3)],6) ~=? knapsack [(2,3),(2,3)] 6

testknpgrader3 =
  let s[]=[];s(e:r)=s[i|i<-r,i<=e]++(e:s[i|i<-r,i>e])in
    [] ~=? [s l|(l,14)<-[knapsack [(2,3),(2,3),(3,4),(3,4),(5,6)] 10],s l/= [(2,3),(2,3),(3,4),(3,4)]]

testbin1 = 6 ~=? (binomDyn (4,2))

tests = TestList
  [ "knapsack 1"   ~: testknp1
  , "knp grader 1" ~: testknpgrader1
  , "knp grader 2" ~: testknpgrader2
  , "knp grader 3" ~: testknpgrader3
  , "binomial 1"   ~: testbin1
  ]

main = do
  runTestTT tests

-- vim:sts=2:sw=2
