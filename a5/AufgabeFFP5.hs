module AufgabeFFP5 where

import Data.Array

mas :: Array Int Int -> Int
mas _ = 0

amas :: Array Int Int -> [(Int,Int)]
amas _ = []

lmas :: Array Int Int -> (Int,Int)
lmas _ = (0,0)

minIndex :: (Ix a, Show a) => Array a b -> (b -> Bool) -> a
minIndex array _ = head $ indices array
