module AufgabeFFP5 where

import Data.Array

mas :: Array Int Int -> Int
mas = mas' . elems
  where
    mas' []     = 0
    mas' (x:xs) = max (mas' xs) (x + mrest xs)
    mrest []      = 0
    mrest (x:xs) = max 0 (x + mrest xs)

amas :: Array Int Int -> [(Int,Int)]
amas _ = []

lmas :: Array Int Int -> (Int,Int)
lmas _ = (0,0)

minIndex :: (Ix a, Show a) => Array a b -> (b -> Bool) -> a
minIndex array _ = head $ indices array
