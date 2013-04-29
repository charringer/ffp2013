module AufgabeFFP5 where

import Data.Array
import Control.Monad

sections array = [ (i,j) | i <- indices array, j <- indices array, i <= j ]

as array (i,j) = sum $ elems $ ixmap (i,j) id array

mas :: Array Int Int -> Int
mas array = maximum $ map (as array) (sections array)

amas :: Array Int Int -> [(Int,Int)]
amas array = [ (i,j) | (i,j) <- sections array, as array (i,j) == mas array ]

lmas :: Array Int Int -> (Int,Int)
lmas array = head [ (i,j) | (i,j) <- amas array, j-i == maxLen ]
  where maxLen = maximum $ map (uncurry (flip (-))) (amas array)

divideAndConquer :: (p -> Bool) -> (p -> s) -> (p -> [p]) -> (p -> [s] -> s) -> p -> s
divideAndConquer indiv solve divide combine initPb = dAC initPb
  where
    dAC pb
      | indiv pb = solve pb
      | otherwise = combine pb (map dAC (divide pb))

a@(Just _) ||| _ = a
Nothing ||| b = b

mi_indiv ls = length ls <= 1
mi_solve [(i,b)] = if b then Just i else Nothing
mi_divide (l:ls) = [[l], ls]
mi_combine _ = foldr (|||) Nothing

minIndex :: (Ix a, Show a) => Array a b -> (b -> Bool) -> a
minIndex array wf = case minidx of { Just a -> a; Nothing -> error "No matching index" }
  where minidx = divideAndConquer mi_indiv mi_solve mi_divide mi_combine \
    [ (i, wf (array ! i)) | i <- indices array ]
