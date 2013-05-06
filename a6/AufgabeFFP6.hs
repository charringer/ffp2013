{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module AufgabeFFP6 where
import Data.Array

eval :: Array Int Int -> Array Int (Int -> Int -> Int) -> Int
eval nums ops = eval' nums ops (snd (bounds nums))

eval' nums ops n
  | n == fst (bounds nums) = nums ! n
  | otherwise = (ops ! (n-1)) (eval' nums ops (n-1)) (nums ! n)

yield :: Array Int Int -> Int -> [Array Int (Int -> Int -> Int)]
yield = yield_gtf

yield_bt :: Array Int Int -> Int -> [Array Int (Int -> Int -> Int)]
yield_bt = undefined

yield_gtf :: Array Int Int -> Int -> [Array Int (Int -> Int -> Int)]
yield_gtf = undefined

instance Show (Array Int (Int->Int->Int)) where
  show = undefined
