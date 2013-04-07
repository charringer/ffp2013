{-# LANGUAGE TemplateHaskell #-}

import Test.HUnit

import AufgabeFFP2   


-- part 1: prime pairs
testpps1 = TestCase $ assertEqual "" [(3,5),(5,7),(11,13),(17,19),(29,31),
  (41,43),(59,61),(71,73),(101,103),(107,109)] (take 10 pps)
testpps2 = TestCase $ assertEqual "" (347,349) (pps!!20)
testpps3 = TestCase $ assertEqual "" (809,811) (head (drop 30 pps))

-- part 2: pow

-- part 3: exp

-- part 4: Gödel numbers
testgz1 = TestCase $ assertEqual "" 144 (gz 42)
testgz2 = TestCase $ assertEqual "" 400 (gz 402)


tests = TestList
  [ TestLabel "Test pps 1" testpps1
  , TestLabel "Test pps 2" testpps2
  , TestLabel "Test pps 3" testpps3
  , TestLabel "Test gz 1" testgz1
  , TestLabel "Test gz 2" testgz2
  ]

main = do
  runTestTT tests

-- vim:sts=2:sw=2
