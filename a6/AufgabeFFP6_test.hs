{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverlappingInstances #-}

import AufgabeFFP6

import Test.HUnit
import Data.Array
import Control.Exception

-- fixtures

nums :: Array Int Int
ops1, ops2, ops3 :: Array Int (Int -> Int -> Int)

nums = array (1,3) [(1,1),(2,2),(3,3)]
ops1 = array (1,2) [(1,(+)),(2,(-))]
ops2 = (array (1,2) [(1,(*)),(2,(+))])
ops3 = (array (1,2) [(1,(-)),(2,(*))])

op1, op2, op3, op4 :: Array Int (Int -> Int -> Int)
op1 = array (1,1) [(1,(+))]
op2 = array (1,1) [(1,(-))]
op3 = array (1,1) [(1,(*))]
op4 = array (1,1) [(1,div)]

-- tests
 
tests = TestList

  ["eval 1"  ~: 0 ~=? eval nums ops1
  ,"eval 2"  ~: 5 ~=? eval nums ops2
  ,"eval 3"  ~: -3 ~=? eval nums ops3

  ,"yield 1" ~: all (== 6) (map (eval nums) (yield nums 6)) ~? "eval . yield != id"
  ,"yield 2" ~: null (yield nums 4) ~? "yield nums 4 == []"
  ,"yield 3" ~: all (== 0) (map (eval nums) (yield nums 0)) ~? "eval . yield != id"

  ,"show 1" ~: "plus" ~=? show op1
  ,"show 2" ~: "minus" ~=? show op2
  ,"show 3" ~: "times" ~=? show op3
  ,"show 4" ~: "div" ~=? show op4
  ]

main = do
  runTestTT tests
